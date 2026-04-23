package dotty
package tools
package dotc

import org.junit.{ AfterClass, Test }
import org.junit.experimental.categories.Category

import vulpix.*

@Category(Array(classOf[ScalaPyCompilationTests]))
class ScalaPyCompilationTests:
  import ParallelTesting.*
  import TestConfiguration.*
  import ScalaPyCompilationTests.*

  @Test def runScalaPy: Unit =
    implicit val testGroup: TestGroup = TestGroup("runScalaPy")
    cleanScalaPyOutput(testGroup)
    val filter = FileFilter.exclude(PylibTest.rawPylibEntries)
    if hasScalaPySources("tests/pos-py", filter) then
      compileFilesInDir("tests/pos-py", scalaPyOptions, filter).checkRuns()

  @Test def negScalaPy: Unit =
    if !hasScalaPySources("tests/neg-py") then
      return
    implicit val testGroup: TestGroup = TestGroup("negScalaPy")
    cleanScalaPyOutput(testGroup)
    compileFilesInDir("tests/neg-py", scalaPyNegOptions).checkExpectedErrors()

object ScalaPyCompilationTests extends ScalaPyTestSuite:
  @AfterClass def tearDown(): Unit =
    finishScalaPySuite()
