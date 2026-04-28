package dotty.tools.dotc

import java.io.File
import java.nio.file.Files
import java.util.Comparator

import scala.concurrent.duration.*

import dotty.Properties
import dotty.tools.TestSources
import dotty.tools.ToolArgs
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.vulpix.*

private[dotc] trait ScalaPyTestSuite extends ParallelTesting:
  implicit val summaryReport: SummaryReporting = new SummaryReport

  def maxDuration = 60.seconds
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
