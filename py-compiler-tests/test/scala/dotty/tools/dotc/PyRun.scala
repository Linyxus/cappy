package dotty.tools.dotc

import java.io.File
import java.io.IOException
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.Duration

import dotty.tools.vulpix.Status

/** Executes a bundled Python file and captures its output. */
object PyRun:

  private final case class ProcessResult(exitCode: Int, output: String, timedOut: Boolean = false)

  private lazy val resolvedProject: Either[String, File] =
    val startDirOrFailure: Either[String, File] =
      try Right(new File(System.getProperty("user.dir")).getCanonicalFile.nn)
      catch case e: IOException =>
        Left(s"Could not resolve user.dir to a canonical File: ${e.getMessage}")
    startDirOrFailure.flatMap { startDir =>
      findProjectRoot(startDir) match
        case None =>
          Left(
            s"Unable to locate the ScalaPy uv project root from ${startDir.getAbsolutePath}\n" +
            "Expected to find both `pyproject.toml` and `uv.lock` in an ancestor directory."
          )
        case Some(root) =>
          ensureSyncedUvProject(root) match
            case None          => Right(root)
            case Some(failure) => Left(failure)
    }

  def runPyCode(classPath: String, maxDuration: Duration = Duration.Inf): Status =
    // The classPath is a colon-separated list; the first entry is the output directory
    val outDir = new File(classPath.split(File.pathSeparator).nn.head.nn)
    val rawListing: Array[File] = outDir.listFiles() match
      case null => Array.empty[File]
      case arr  => arr.asInstanceOf[Array[File]]
    val pyFiles = rawListing.filter(_.getName.nn.endsWith(".py"))

    if pyFiles.isEmpty then
      return Status.Failure("No .py files found in output directory: " + outDir)

    // Pick the bundled .py file (there should be exactly one after linking)
    val pyFile = pyFiles.head

    resolvedProject match
      case Left(failure) =>
        Status.Failure(failure)
      case Right(projectRoot) =>
        runProcess(
          List(
            "uv", "run",
            "--project", projectRoot.getAbsolutePath,
            "--no-sync",
            // -W ignore suppresses Python SyntaxWarnings ("`is` with int
            // literal" etc.) so they don't pollute test output. The
            // backend's emitted code occasionally trips these (the
            // semantics are correct — `is` on a small int interns and
            // works — but the warning text is noise on stdout/stderr).
            "python", "-W", "ignore", pyFile.getAbsolutePath
          ),
          outDir,
          projectRoot,
          maxDuration
        ) match
          case Left(failure) =>
            Status.Failure(failure)
          case Right(result) if result.timedOut =>
            Status.Failure(s"Python subprocess exceeded maxDuration=$maxDuration and was killed.\nPartial output:\n${result.output}")
          case Right(result) =>
            if result.exitCode == 0 then Status.Success(result.output)
            else Status.Failure(result.output)

  private[dotc] def findProjectRoot(startDir: File): Option[File] =
    Iterator.iterate(startDir.getCanonicalFile)(_.getParentFile)
      .takeWhile(_ != null)
      .find(dir =>
        new File(dir, "pyproject.toml").isFile &&
        new File(dir, "uv.lock").isFile
      )

  private[dotc] def formatSyncFailure(projectRoot: File, output: String): String =
    val trimmedOutput = output.trim
    val renderedOutput =
      if trimmedOutput.isEmpty then "uv reported no additional output."
      else trimmedOutput
    s"""|ScalaPy generated Python must run against the repo's locked uv environment.
        |The project environment is not synchronized for:
        |  ${projectRoot.getAbsolutePath}
        |
        |Run `uv sync --frozen` in the repo root, then rerun:
        |  sbt --client "pyCompilerTests/test"
        |
        |uv sync --check output:
        |$renderedOutput
        |""".stripMargin

  private def ensureSyncedUvProject(projectRoot: File): Option[String] =
    runProcess(
      List("uv", "sync", "--project", projectRoot.getAbsolutePath, "--frozen", "--check"),
      projectRoot,
      projectRoot,
      Duration.Inf
    ) match
      case Left(failure) =>
        Some(failure)
      case Right(result) if result.exitCode == 0 =>
        None
      case Right(result) =>
        Some(formatSyncFailure(projectRoot, result.output))

  private def runProcess(command: List[String], workingDirectory: File, projectRoot: File, maxDuration: Duration): Either[String, ProcessResult] =
    try
      val pb = new ProcessBuilder(command*)
        .directory(workingDirectory)
        .redirectErrorStream(true)
      val extras = new File(workingDirectory, "_pyextras")
      if extras.isDirectory then
        val env = pb.environment().nn
        val prior = env.get("PYTHONPATH")
        val combined =
          if prior == null || prior.isEmpty
          then extras.getAbsolutePath
          else extras.getAbsolutePath + File.pathSeparator + prior
        env.put("PYTHONPATH", combined)
      val process = pb.start()

      // Self-enforce a deadline because Vulpix's per-fixture maxDuration
      // is not propagated through ScalaPyTestSuite.runMain (which calls us
      // synchronously, bypassing the runner-pool Future + Await pattern
      // in RunnerOrchestration). A watchdog thread destroys the child on
      // deadline; the main thread keeps draining stdout (which prevents
      // the subprocess from blocking on a full pipe buffer) and observes
      // EOF naturally when the process exits or is killed.
      val killedByWatchdog = new java.util.concurrent.atomic.AtomicBoolean(false)
      val watchdog: Option[Thread] =
        if maxDuration.isFinite then
          val t = new Thread(() => {
            try
              java.lang.Thread.sleep(maxDuration.toMillis)
              if process.isAlive then
                killedByWatchdog.set(true)
                process.destroy()
                if !process.waitFor(2L, java.util.concurrent.TimeUnit.SECONDS) then
                  process.destroyForcibly()
            catch
              case _: InterruptedException => () // process exited cleanly first
          }, "PyRun-watchdog")
          t.setDaemon(true)
          t.start()
          Some(t)
        else None

      val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      var interrupted = false
      var exitCode    = 0
      var done        = false
      while !done do
        try
          exitCode = process.waitFor()
          done = true
        catch
          case _: InterruptedException =>
            interrupted = true
            process.destroy()
            if !process.waitFor(2L, java.util.concurrent.TimeUnit.SECONDS) then
              process.destroyForcibly()
              exitCode = process.waitFor()
            else
              exitCode = process.exitValue()
            done = true
      watchdog.foreach(_.interrupt())
      if interrupted then Thread.currentThread.nn.interrupt()
      Right(ProcessResult(exitCode, output, timedOut = killedByWatchdog.get()))
    catch
      case e: IOException =>
        Left(
          s"""|Failed to start `${command.headOption.getOrElse("process")}` while executing ScalaPy generated code.
              |Project root: ${projectRoot.getAbsolutePath}
              |Command: ${command.mkString(" ")}
              |
              |Ensure `uv` is installed and the project environment is prepared:
              |  uv sync --frozen
              |
              |Cause: ${e.getMessage}
              |""".stripMargin
        )
