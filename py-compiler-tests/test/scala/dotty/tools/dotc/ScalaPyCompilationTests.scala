package dotty
package tools
package dotc

import java.io.File
import java.nio.file.Files
import java.util.Comparator

import org.junit.{ Test, AfterClass }
import org.junit.experimental.categories.Category

import scala.concurrent.duration.*
import reporting.TestReporter
import vulpix.*

@Category(Array(classOf[ScalaPyCompilationTests]))
class ScalaPyCompilationTests:
  import ParallelTesting.*
  import TestConfiguration.*
  import ScalaPyCompilationTests.*
  import CompilationTest.aggregateTests

  @Test def runScalaPy: Unit =
    implicit val testGroup: TestGroup = TestGroup("runScalaPy")
    cleanScalaPyOutput(testGroup)
    aggregateTests(
      compileFilesInDir("tests-py", scalaPyOptions),
    ).checkRuns()

  @Test def negScalaPy: Unit =
    if !hasScalaPySources("tests/py-neg") then
      return
    implicit val testGroup: TestGroup = TestGroup("negScalaPy")
    cleanScalaPyOutput(testGroup)
    aggregateTests(
      compileFilesInDir("tests/py-neg", scalaPyOptions),
    ).checkExpectedErrors()

object ScalaPyCompilationTests extends ParallelTesting:
  implicit val summaryReport: SummaryReporting = new SummaryReport

  def maxDuration = 60.seconds
  def numberOfWorkers = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  private def cleanScalaPyOutput(testGroup: TestGroup): Unit =
    val outDir = new File(ParallelTesting.defaultOutputDir, testGroup.name)
    if outDir.exists then
      val paths = Files.walk(outDir.toPath)
      try
        paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally
        paths.close()

  private def hasScalaPySources(dir: String): Boolean =
    val root = new File(dir)
    root.exists && root.isDirectory && root.listFiles().exists(_.getName.endsWith(".scala"))

  @AfterClass def tearDown(): Unit =
    cleanup()
    summaryReport.echoSummary()

  override def runMain(classPath: String, toolArgs: ToolArgs)(implicit summaryReport: SummaryReporting): Status =
    try
      PyRun.runPyCode(classPath)
    catch
      case t: Exception =>
        val writer = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(writer))
        Status.Failure(writer.toString())
