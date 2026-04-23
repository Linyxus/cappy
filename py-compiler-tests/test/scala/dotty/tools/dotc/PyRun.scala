package dotty.tools.dotc

import java.io.File
import java.io.IOException
import java.nio.charset.StandardCharsets

import dotty.tools.vulpix.Status

/** Executes a bundled Python file and captures its output. */
object PyRun:

  private final case class ProcessResult(exitCode: Int, output: String)

  def runPyCode(classPath: String): Status =
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

    val projectRoot =
      findProjectRoot(outDir).getOrElse {
        return Status.Failure(
          s"Unable to locate the ScalaPy uv project root from output directory: ${outDir.getAbsolutePath}\n" +
          "Expected to find both `pyproject.toml` and `uv.lock` in an ancestor directory."
        )
      }

    ensureSyncedUvProject(projectRoot) match
      case Some(failure) =>
        Status.Failure(failure)
      case None =>
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
          projectRoot
        ) match
          case Left(failure) =>
            Status.Failure(failure)
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
      projectRoot
    ) match
      case Left(failure) =>
        Some(failure)
      case Right(result) if result.exitCode == 0 =>
        None
      case Right(result) =>
        Some(formatSyncFailure(projectRoot, result.output))

  private def runProcess(command: List[String], workingDirectory: File, projectRoot: File): Either[String, ProcessResult] =
    try
      val process = new ProcessBuilder(command*)
        .directory(workingDirectory)
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      // ForkJoinPool can interrupt workers in blocking I/O while reshuffling
      // near pool shutdown (see `notes/issue-pyrun-waitfor-interrupt.md`).
      // Loop until the subprocess exits; preserve the interrupt flag for any
      // legitimate cancellation path upstream.
      var interrupted = false
      var exitCode    = 0
      var done        = false
      while !done do
        try
          exitCode = process.waitFor()
          done = true
        catch
          case _: InterruptedException => interrupted = true
      if interrupted then Thread.currentThread.nn.interrupt()
      Right(ProcessResult(exitCode, output))
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
