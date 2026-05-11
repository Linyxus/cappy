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
@Category(Array(classOf[PyStockRunTests]))
class PyStockRunTests:
  import ParallelTesting.*
  import TestConfiguration.*
  import PyStockRunTests.*

  // The full tests/run/ sweep is too large to finish under Vulpix's
  // hard-coded 20-minute executeTestSuite cap. Earlier first-letter
  // partitioning was too coarse: heavy fixtures cluster on a few letters
  // (e.g. `t`, `p`), so those chunks blew the budget while `i` finished in
  // ~4 min. Instead, hash-bucket fixture names into NumBuckets chunks via
  // Java's deterministic String#hashCode + floorMod, which mixes heavy and
  // light fixtures across all buckets so no single one is dominated by the
  // slow tail. See notes/issue-vulpix-test-suite-timeout.md.
  //
  // NumBuckets = 16 yields ~107 fixtures/chunk for ~1708 fixtures. An
  // earlier 8-bucket attempt still hit Vulpix's 20-min cap on one chunk
  // (heavy fixtures clustered by hash) and OOM'd another at -Xmx16g; halving
  // the bucket size keeps each chunk's worst-case heap and time well below
  // the budget.

  @Test def runPyTests_00: Unit = runPyTestsChunk("runPyTests/00", bucketFilter(0))
  @Test def runPyTests_01: Unit = runPyTestsChunk("runPyTests/01", bucketFilter(1))
  @Test def runPyTests_02: Unit = runPyTestsChunk("runPyTests/02", bucketFilter(2))
  @Test def runPyTests_03: Unit = runPyTestsChunk("runPyTests/03", bucketFilter(3))
  @Test def runPyTests_04: Unit = runPyTestsChunk("runPyTests/04", bucketFilter(4))
  @Test def runPyTests_05: Unit = runPyTestsChunk("runPyTests/05", bucketFilter(5))
  @Test def runPyTests_06: Unit = runPyTestsChunk("runPyTests/06", bucketFilter(6))
  @Test def runPyTests_07: Unit = runPyTestsChunk("runPyTests/07", bucketFilter(7))
  @Test def runPyTests_08: Unit = runPyTestsChunk("runPyTests/08", bucketFilter(8))
  @Test def runPyTests_09: Unit = runPyTestsChunk("runPyTests/09", bucketFilter(9))
  @Test def runPyTests_10: Unit = runPyTestsChunk("runPyTests/10", bucketFilter(10))
  @Test def runPyTests_11: Unit = runPyTestsChunk("runPyTests/11", bucketFilter(11))
  @Test def runPyTests_12: Unit = runPyTestsChunk("runPyTests/12", bucketFilter(12))
  @Test def runPyTests_13: Unit = runPyTestsChunk("runPyTests/13", bucketFilter(13))
  @Test def runPyTests_14: Unit = runPyTestsChunk("runPyTests/14", bucketFilter(14))
  @Test def runPyTests_15: Unit = runPyTestsChunk("runPyTests/15", bucketFilter(15))

  private def runPyTestsChunk(group: String, chunkFilter: FileFilter): Unit =
    implicit val testGroup: TestGroup = TestGroup(group)
    cleanScalaPyOutput(testGroup)
    val filter = FileFilter.and(chunkFilter, FileFilter.exclude(loadExcludelist(excludelistFile)))
    if hasScalaPySources("tests/run", filter) then
      stagePythonCompanions(compileFilesInDir("tests/run", scalaPyOptions, filter)).checkRuns()

  private def bucketFilter(bucket: Int): FileFilter =
    FileFilter.predicate(name => Math.floorMod(name.hashCode, NumBuckets) == bucket)

object PyStockRunTests extends ScalaPyTestSuite:
  // Lower parallelism for the run-py sweep. The PyReachability memoization
  // (Layer 0.3) helps, but 5 parallel Ycheck/genPython phases on heavy
  // fixtures (e.g. poly-kinded-derives, partialFunctions) still exhaust
  // -Xmx16g. Treat any further heap work as a deeper investigation
  // separate from Layer 0; for now keep workers at 2.
  override def numberOfWorkers: Int = 2

  /** Number of hash buckets the tests/run/ sweep is partitioned into. Each
   *  bucket runs as its own @Test method so it gets its own 20-min Vulpix
   *  executeTestSuite budget. Tune upward if any bucket regresses past ~18 min.
   */
  private[dotc] val NumBuckets: Int = 16

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
