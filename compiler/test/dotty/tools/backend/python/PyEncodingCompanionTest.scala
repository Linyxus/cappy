package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Tests for the class-vs-companion encoded-name relation in
 *  `PyEncoding.companionClassNameOf` and `PyEncoding.isModuleClassName`.
 *
 *  Scenario: when a Scala source identifier itself ends with `$` (e.g.
 *  `class abc$` paired with `object abc$`), plain `$ -> _` sanitization
 *  used to flatten the class's encoded name onto the suffix slot that
 *  the companion-flip relation assigned to the module's encoded name.
 *  The linker then rejected the duplicate (`Duplicate class
 *  'C___abc_'`). The encoder now escapes a non-module class's trailing
 *  user `$` with the reserved `_scpy_d` marker so the relation is
 *  collision-free by construction. */
class PyEncodingCompanionTest:

  // -----------------------------------------------------------------
  //  Companion-flip round-trip — standard case
  // -----------------------------------------------------------------

  @Test def standardModuleAndClassFlip(): Unit =
    val cls    = PyClassName("Foo")
    val module = PyClassName("Foo_")
    assertEquals(module, PyEncoding.companionClassNameOf(cls))
    assertEquals(cls,    PyEncoding.companionClassNameOf(module))
    assertFalse(PyEncoding.isModuleClassName(cls))
    assertTrue(PyEncoding.isModuleClassName(module))

  // -----------------------------------------------------------------
  //  Companion-flip round-trip — user-source name ends in `$`
  //  (the t6888 scenario, lifted to the name level).
  // -----------------------------------------------------------------

  @Test def trailingDollarClassDoesNotCollideWithCompanionModule(): Unit =
    // `class abc$` and `object abc$` are companions. Pre-fix, both
    // mapped onto a single `_`-trailing encoded name slot; the linker
    // emitted `Duplicate class`. Post-fix, the regular class's
    // encoded simple name carries the reserved `_scpy_d` marker, the
    // module class keeps the plain `_` suffix, and the two are
    // distinct PyClassNames — yet still companion-flippable.
    val cls    = PyClassName("abc_scpy_d")
    val module = PyClassName("abc__")
    assertNotEquals(cls, module)
    assertEquals(module, PyEncoding.companionClassNameOf(cls))
    assertEquals(cls,    PyEncoding.companionClassNameOf(module))

  @Test def trailingDollarClassIsNotMisclassifiedAsModule(): Unit =
    // The whole point of the `_scpy_d` escape is that a regular class
    // whose source name ended in `$` does NOT alias to a module class
    // slot. `isModuleClassName` must agree.
    val cls = PyClassName("abc_scpy_d")
    assertFalse(
      "abc_scpy_d (class abc$) must not be classified as a module class",
      PyEncoding.isModuleClassName(cls)
    )

  @Test def companionModuleOfTrailingDollarClassIsAModule(): Unit =
    val module = PyClassName("abc__")
    assertTrue(PyEncoding.isModuleClassName(module))

  // -----------------------------------------------------------------
  //  Distinct symbols (class, companion module, inner class) yield
  //  pairwise-distinct encoded names — the property the linker relies
  //  on to reject true duplicates without flagging the t6888 trio.
  // -----------------------------------------------------------------

  @Test def classCompanionInnerTrioIsPairwiseDistinct(): Unit =
    // Models the t6888 trio for an outer class `C` carrying a
    // `class abc$` and `object abc$` (with an inner class inside the
    // object for added stress).
    //
    // Encoder outputs (after the `$ -> _` plus `_scpy_d` rules):
    //   class    C.abc$           -> "C.abc_scpy_d"
    //   module   C.object abc$    -> "C.abc__"
    //   inner    C.object abc$.X  -> "C.abc__.X"
    val regularClass    = PyClassName("C.abc_scpy_d")
    val companionModule = PyClassName("C.abc__")
    val innerOfModule   = PyClassName("C.abc__.X")

    val all = List(regularClass, companionModule, innerOfModule)
    for a <- all; b <- all if a ne b do
      assertNotEquals(
        s"distinct symbols must encode to distinct PyClassNames: $a == $b",
        a, b
      )

    // And the companion-flip lands on the partner, never on the inner.
    assertEquals(companionModule, PyEncoding.companionClassNameOf(regularClass))
    assertEquals(regularClass,    PyEncoding.companionClassNameOf(companionModule))
    assertNotEquals(innerOfModule, PyEncoding.companionClassNameOf(regularClass))
    assertNotEquals(innerOfModule, PyEncoding.companionClassNameOf(companionModule))

  // -----------------------------------------------------------------
  //  Field-companion derivation reuses companionClassNameOf and
  //  therefore inherits the `_scpy_d` handling.
  // -----------------------------------------------------------------

  @Test def companionFieldOfClassWithTrailingDollar(): Unit =
    val classField = PyFieldName(
      PyClassName("abc_scpy_d"),
      PySimpleFieldName("x")
    )
    val moduleField = PyEncoding.companionFieldOf(classField)
    assertEquals(PyClassName("abc__"), moduleField.owner)
    assertEquals(classField.simple, moduleField.simple)

end PyEncodingCompanionTest
