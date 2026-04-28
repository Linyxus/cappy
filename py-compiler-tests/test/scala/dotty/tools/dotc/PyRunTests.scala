package dotty.tools.dotc

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import org.junit.{ AfterClass, Test }
import org.junit.experimental.categories.Category

import dotty.tools.TestSources
import dotty.tools.vulpix.*

/** Runs the Python backend against fixtures in `tests/run/`.
 *
 *  Uses an excludelist (initially empty): every fixture is submitted to
 *  compilation/linking/execution unless explicitly listed in
 *  `py-compiler-tests/test/run-py-tests.excludelist`. Failures are the
 *  intended signal — they surface backend bugs and missing features.
 *  Exempt only fixtures that are confirmed unsupportable, with a `# reason`
 *  tag so the exclusion is auditable.
 */
@Category(Array(classOf[PyRunTests]))
class PyRunTests:
  import ParallelTesting.*
  import TestConfiguration.*
  import PyRunTests.*

  // The full tests/run/ sweep is too large to finish under Vulpix's
  // hard-coded 20-minute executeTestSuite cap. Partition by the first
  // character of the fixture name so each chunk gets its own budget.
  // See notes/issue-vulpix-test-suite-timeout.md.

  @Test def runPyTests_t: Unit =
    runPyTestsChunk("runPyTests/t", FileFilter.predicate(startsWithLetter(_, 't')))

  @Test def runPyTests_i: Unit =
    runPyTestsChunk("runPyTests/i", FileFilter.predicate(startsWithLetter(_, 'i')))

  @Test def runPyTests_other: Unit =
    runPyTestsChunk("runPyTests/other", FileFilter.predicate(name => !startsWithLetter(name, 't') && !startsWithLetter(name, 'i')))

  private def runPyTestsChunk(group: String, chunkFilter: FileFilter): Unit =
    implicit val testGroup: TestGroup = TestGroup(group)
    cleanScalaPyOutput(testGroup)
    val filter = FileFilter.and(chunkFilter, FileFilter.exclude(loadExcludelist(excludelistFile)))
    if hasScalaPySources("tests/run", filter) then
      compileFilesInDir("tests/run", scalaPyOptions, filter).checkRuns()

  private def startsWithLetter(name: String, letter: Char): Boolean =
    name.nonEmpty && Character.toLowerCase(name.charAt(0)) == letter

object PyRunTests extends ScalaPyTestSuite:
  // Lower parallelism for the run-py sweep. The PyReachability memoization
  // (Layer 0.3) helps, but 5 parallel Ycheck/genPython phases on heavy
  // fixtures (e.g. poly-kinded-derives, partialFunctions) still exhaust
  // -Xmx16g. Treat any further heap work as a deeper investigation
  // separate from Layer 0; for now keep workers at 2.
  override def numberOfWorkers: Int = 2

  private val excludelistFile: String = "py-compiler-tests/test/run-py-tests.excludelist"

  /** Read an excludelist file, applying the same comment/blank-line rules as
   *  `dotty.tools.TestSources.loadList` (which is private). A missing file
   *  yields an empty list so a fresh checkout still works.
   */
  private[dotc] def loadExcludelist(path: String): List[String] =
    val resolved: Path = TestSources.getPath(path)
    if !Files.exists(resolved) then Nil
    else
      Files.readAllLines(resolved).asScala.iterator
        .map(_.trim)
        .filterNot(_.startsWith("#"))
        .map(_.takeWhile(_ != '#').trim)
        .filter(_.nonEmpty)
        .toList

  @AfterClass def tearDown(): Unit =
    finishScalaPySuite()
