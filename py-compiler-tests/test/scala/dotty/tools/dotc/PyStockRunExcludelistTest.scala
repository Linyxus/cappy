package dotty
package tools
package dotc

import org.junit.Assert.*
import org.junit.Test

/** Sanity checks for `py-compiler-tests/test/run-py-tests.excludelist`.
 *
 *  The full `tests/run/` sweep is partitioned into hash buckets that take
 *  several minutes apiece, so it is not practical to verify every
 *  excludelist entry through the live harness. Instead, exercise the
 *  parser used by `PyStockRunTests.loadExcludelist` against the on-disk
 *  file and assert that representative fixtures from the
 *  `jvm-reflection-not-supported` cluster (Wave 5 priority #1, see
 *  `notes/wave5-worklist/01-reflection-unsupported-and-blacklist.md`)
 *  reach the parsed list. If the file is moved, renamed, or its parser
 *  is changed in a way that drops these entries, this test fires
 *  instead of the entire bucket silently flipping back to red.
 */
class PyStockRunExcludelistTest:

  @Test def parserPicksUpJvmReflectionExclusions(): Unit =
    val parsed = PyStockRunTests.loadExcludelist(
      "py-compiler-tests/test/run-py-tests.excludelist"
    )
    assertTrue(
      s"Excludelist parser returned no entries; expected >= 50",
      parsed.sizeIs >= 50
    )
    // Representative fixtures from each subcluster of the
    // jvm-reflection-not-supported group.
    val mustContain = List(
      "i9404.scala",                  // scala.reflect.Selectable / getField
      "paramForwarding.scala",        // getDeclaredFields
      "i18701.scala",                 // getEnclosingMethod
      "t7269.scala",                  // getDeclaredMethods
      "structural.scala",             // structural Selectable family
      "weak-conformance.scala"        // tail of cluster, alphabetical sanity
    )
    for name <- mustContain do
      assertTrue(
        s"Expected '$name' in excludelist (jvm-reflection-not-supported)",
        parsed.contains(name)
      )

  @Test def perfBPrimeEntriesStillPresent(): Unit =
    val parsed = PyStockRunTests.loadExcludelist(
      "py-compiler-tests/test/run-py-tests.excludelist"
    )
    val perfBPrime = List(
      "t8893.scala",
      "t3502.scala",
      "collections.scala",
      "kmpSliceSearch.scala"
    )
    for name <- perfBPrime do
      assertTrue(
        s"Expected '$name' in excludelist (Perf B' cluster); the parser " +
          "must not drop legacy entries when adding new ones.",
        parsed.contains(name)
      )

  @Test def commentLinesAndReasonTagsAreStripped(): Unit =
    val parsed = PyStockRunTests.loadExcludelist(
      "py-compiler-tests/test/run-py-tests.excludelist"
    )
    // The header has lines like `# --- jvm-reflection-not-supported ...`
    // and every entry has a trailing `# reason` tag. Neither the leading
    // nor the trailing comment should leak into the parsed list.
    for entry <- parsed do
      assertFalse(s"entry contains '#': $entry", entry.contains("#"))
      assertFalse(s"entry has trailing whitespace: '$entry'", entry != entry.trim)
      assertTrue(s"entry is empty", entry.nonEmpty)
