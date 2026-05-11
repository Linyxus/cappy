package dotty.tools.dotc

import org.junit.{ AfterClass, Test }
import org.junit.experimental.categories.Category

import dotty.tools.vulpix.*

@Category(Array(classOf[PylibTest]))
class PylibTest:
  import ParallelTesting.*
  import TestConfiguration.*
  import PylibTest.*

  @Test def runScalaPyPylib: Unit =
    implicit val testGroup: TestGroup = TestGroup("runScalaPyPylib")
    cleanScalaPyOutput(testGroup)
    val filter = FileFilter.include(rawPylibEntries)
    if hasScalaPySources("tests/pos-py", filter) then
      stagePythonCompanions(compileFilesInDir("tests/pos-py", scalaPyRawPylibOptions, filter)).checkRuns()

object PylibTest extends ScalaPyTestSuite:
  val rawPylibEntries: List[String] = List(
    "javalib-throwables-chain.scala",
    "javalib-math-roundingmode.scala",
    "javalib-concurrent-timeunit.scala",
    "javalib-util-collections",
    "javalib-lang-bounds-checks.scala",
    "javalib-charset-convenience.scala",
    "javalib-throwables-io.scala",
    "javalib-lang-stringbuffer.scala",
    "javalib-markers-iterators",
    "javalib-lang-string-charset.scala",
    "javalib-util-formatter",
    "javalib-util-internal.scala",
    "javalib-math-biginteger.scala",
    "javalib-util-collection-contracts",
    "javalib-concurrent-collections.scala",
    "javalib-util-objects",
    "javalib-math-mathcontext.scala",
    "javalib-charset-roundtrips.scala",
    "pyfacade-utils.scala",
    "javalib-markers-core",
    "javalib-throwables-stacktrace.scala",
    "javalib-charset-core.scala",
    "javalib-math-extras.scala",
    "javalib-io-system-streams.scala",
    "javalib-lang-reflect-array.scala",
    "javalib-markers-sequences",
    "javalib-lang-stringbuilder.scala",
    "javalib-inheritablethreadlocal-copy.scala",
    "javalib-math-bigdecimal.scala",
    "javalib-util-arrays",
    "javalib-net-urlencoder.scala",
    "javalib-lang-boxed-numbers.scala",
  )

  @AfterClass def tearDown(): Unit =
    finishScalaPySuite()
