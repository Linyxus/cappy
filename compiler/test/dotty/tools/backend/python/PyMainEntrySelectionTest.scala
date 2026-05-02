package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

import scala.collection.mutable

/** Drives `PyCodeGenSupport.pickMainEntry` against synthetic candidate sets.
 *
 *  Regression coverage for the non-determinism bug in `tests/run/main-functions.scala`:
 *  before the fix, `GenPython` overwrote `mainEntry` on every match in the
 *  type-def traversal, so whichever `@main`-bearing class arrived last from
 *  the pipeline won. Across local rebuilds this flipped between `Test` and
 *  `foo`. The fix collects every candidate and picks deterministically;
 *  these tests pin that contract.
 */
class PyMainEntrySelectionTest:

  private val ClassKind = PyClassKind.Class
  private val ModuleKind = PyClassKind.ModuleClass

  private def candidate(name: String, kind: PyClassKind = ClassKind): PyIREmitter.MainEntry =
    (PyClassName(name), kind)

  private def noWarn: (List[String], String) => Unit =
    (_, _) => fail("ambiguity callback should not fire")

  private def recording(): (
      mutable.ListBuffer[(List[String], String)],
      (List[String], String) => Unit
  ) =
    val log = mutable.ListBuffer.empty[(List[String], String)]
    (log, (names, chosen) => log += ((names, chosen)))

  @Test def returnsNoneWhenNoCandidates(): Unit =
    val chosen = PyCodeGenSupport.pickMainEntry(Nil, "", noWarn)
    assertEquals(None, chosen)

  @Test def returnsSoleCandidateUnchanged(): Unit =
    val only = candidate("Test")
    val chosen = PyCodeGenSupport.pickMainEntry(List(only), "", noWarn)
    assertEquals(Some(only), chosen)

  @Test def picksLexicographicallyFirstAcrossOrderings(): Unit =
    // `tests/run/main-functions.scala` shape: top-level `Test` and a `foo`
    // proxy generated from `object A { @main def foo }`. Capital `T` (84)
    // sorts before lowercase `f` (102) in ASCII, matching the convention
    // vulpix uses (`Test.main` is the test runner's entry point).
    val test  = candidate("Test")
    val foo   = candidate("foo", ModuleKind)
    val expected = Some(test)

    // Permute the input order; the chosen entry must not change.
    val orderings = List(
      List(test, foo),
      List(foo, test)
    )
    for input <- orderings do
      val (log, sink) = recording()
      val chosen = PyCodeGenSupport.pickMainEntry(input, "", sink)
      assertEquals(s"input order: $input", expected, chosen)
      // Ambiguity callback should fire exactly once per call, with the
      // sorted candidate list (so callers get stable diagnostic text).
      assertEquals(1, log.size)
      assertEquals(List("Test", "foo"), log.head._1)
      assertEquals("Test", log.head._2)

  @Test def deterministicAcrossRepeatedInvocations(): Unit =
    val test = candidate("Test")
    val foo  = candidate("foo", ModuleKind)
    val baz  = candidate("Baz")
    val input = List(foo, test, baz)

    val results = (1 to 5).map { _ =>
      val (_, sink) = recording()
      PyCodeGenSupport.pickMainEntry(input, "", sink)
    }.toList

    // All five invocations must return the same Option[MainEntry].
    assertEquals(1, results.distinct.size)
    // And that answer is the lexicographic minimum: `Baz` < `Test` < `foo`.
    assertEquals(Some(baz), results.head)

  @Test def explicitNameOverridesSortWhenMatching(): Unit =
    val test = candidate("Test")
    val foo  = candidate("foo", ModuleKind)
    val (log, sink) = recording()
    val chosen = PyCodeGenSupport.pickMainEntry(List(test, foo), "foo", sink)
    assertEquals(Some(foo), chosen)
    // Ambiguity callback still fires — multiple candidates were present.
    assertEquals(1, log.size)
    assertEquals("foo", log.head._2)

  @Test def explicitNameFallsBackToSortWhenNotMatching(): Unit =
    val test = candidate("Test")
    val foo  = candidate("foo", ModuleKind)
    val (log, sink) = recording()
    // `Bogus` doesn't appear in the candidate list, so the rule falls
    // through to the lexicographic sort: `Test` wins.
    val chosen = PyCodeGenSupport.pickMainEntry(List(test, foo), "Bogus", sink)
    assertEquals(Some(test), chosen)
    assertEquals(1, log.size)
    assertEquals("Test", log.head._2)

  @Test def singleCandidateIgnoresExplicitNameWithoutWarning(): Unit =
    val only = candidate("Test")
    val chosen = PyCodeGenSupport.pickMainEntry(List(only), "ignored", noWarn)
    assertEquals(Some(only), chosen)
