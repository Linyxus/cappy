package dotty.tools.dotc

import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.Comparator

import scala.concurrent.duration.*

import dotty.Properties
import dotty.tools.TestPlatform
import dotty.tools.TestSources
import dotty.tools.ToolArgs
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.vulpix.*

private[dotc] trait ScalaPyTestSuite extends ParallelTesting:
  implicit val summaryReport: SummaryReporting = new SummaryReport

  // The Python backend is its own test platform. Two consequences:
  //
  //  - Check-file resolution is `<base>.python.check` → `<base>.check`
  //    (see `ParallelTesting.checkFile`). Where Python output happens
  //    to match JVM's `.check`, no override is needed; where it
  //    diverges, a sibling `.python.check` lives next to the test.
  //
  //  - `//> using target.platform <p>` companion files tagged with
  //    any platform other than `python` are excluded from
  //    compilation (see `ParallelTesting`'s `platformFiles` filter).
  //    Tests that historically relied on a `scala-js`-tagged stub
  //    for a shared symbol need an explicit `python`-tagged sibling.
  override protected def testPlatform: TestPlatform = TestPlatform.Python

  // Bumped from 60s → 90s on 2026-05-02 to cover Perf B fixtures that
  // run correctly but exceed 60s under CPython interpretation
  // (i20145 ~52s pre-Perf-C / ~3s post; t6584 63s; UnrolledBuffer 66s;
  // t2818 43s borderline). See `notes/issue-stockrun-timeout-cluster.md`.
  // Genuine hangs still surface within 90s; fixtures that need >90s
  // (Perf B') are excludelisted with reason tags in
  // `run-py-tests.excludelist`.
  def maxDuration = 90.seconds
  def numberOfWorkers = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  def cleanScalaPyOutput(testGroup: TestGroup): Unit =
    val outDir = new File(ParallelTesting.defaultOutputDir, testGroup.name)
    if outDir.exists then
      val paths = Files.walk(outDir.toPath)
      try
        paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally
        paths.close()

  def hasScalaPySources(dir: String, fileFilter: FileFilter = FileFilter.NoFilter): Boolean =
    val root = TestSources.getPath(dir).toFile
    root.exists &&
    root.isDirectory &&
    root.listFiles().nn.exists(file =>
      fileFilter.accept(file.getName) &&
      (testFilter.isEmpty || testFilter.exists(file.getPath.contains)) &&
      (file.isDirectory || (file.isFile && file.getName.endsWith(".scala")))
    )

  /** Stage Python companion files from directory-style fixtures into
   *  `<outDir>/_pyextras/`. PyRun prepends that subdir to PYTHONPATH at
   *  run time so the bundle can `import <companion>`. Flat-file
   *  (JointCompilationSource) targets are skipped — companions only work
   *  for directory fixtures. Returns the input for fluent chaining.
   */
  def stagePythonCompanions(test: CompilationTest): CompilationTest =
    test.targets.foreach {
      case sep: SeparateCompilationSource =>
        val pyFiles = Option(sep.dir.listFiles()).getOrElse(Array.empty[File])
          .filter(f => f.isFile && f.getName.endsWith(".py"))
        if pyFiles.nonEmpty then
          val extras = new File(sep.outDir, "_pyextras")
          extras.mkdirs()
          pyFiles.foreach { src =>
            Files.copy(
              src.toPath,
              new File(extras, src.getName).toPath,
              StandardCopyOption.REPLACE_EXISTING
            )
          }
      case _ => ()
    }
    test

  protected def finishScalaPySuite(): Unit =
    cleanup()
    summaryReport.echoSummary()

  override def runMain(classPath: String, toolArgs: ToolArgs)(using SummaryReporting): Status =
    try
      // Pass the suite's per-fixture maxDuration through to PyRun so it can
      // self-enforce a deadline. ScalaPyTestSuite.runMain bypasses the JVM
      // runner-pool Future/Await pattern that RunnerOrchestration uses, so
      // PyRun is the only place left to honor maxDuration.
      PyRun.runPyCode(classPath, maxDuration)
    catch
      case t: Exception =>
        val writer = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(writer))
        Status.Failure(writer.toString())
