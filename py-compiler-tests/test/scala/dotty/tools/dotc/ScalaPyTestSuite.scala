package dotty.tools.dotc

import java.io.File
import java.nio.file.Files
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

  // The Python backend exercises shared `tests/run/` fixtures whose
  // separate-compilation siblings (e.g. `unroll-*-integration`) ship
  // both `//> using target.platform jvm` and
  // `//> using target.platform scala-js` files in the same group.
  // The JVM variant uses Java reflection (`getMethod`, `Boolean.FALSE`)
  // to inspect the bytecode-level forwarders synthesized by `@unroll`;
  // the Scala.js variant is intentionally a no-op stub. Both name a
  // shared symbol the platform-neutral test driver references, so
  // exactly one of them must be kept.
  //
  // Selecting the Scala.js variant aligns with the Python backend:
  // there are no JVM bytecode forwarders to reflect on, and the
  // no-op stub keeps the test driver linkable. JVM-only sources
  // would otherwise trip PyIR linker errors (e.g. on
  // `java.lang.Boolean.FALSE`) that have no Python analogue.
  override protected def testPlatform: TestPlatform = TestPlatform.ScalaJS

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
