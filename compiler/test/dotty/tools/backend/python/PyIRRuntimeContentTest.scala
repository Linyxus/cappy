package dotty.tools.backend.python

import org.junit.Assert.*
import org.junit.Test

/** Witness test for `PyIRRuntime.content` — the runtime prelude string.
 *
 *  Phase 4 of `notes/shrink-runtime.md` converts the prelude from
 *  hand-spelled mangled identifiers to interpolated helper calls
 *  (`m`, `f`, `mod`). The substitutions must produce byte-identical
 *  output. This test anchors a few canonical encoded names so the
 *  refactor cannot silently lose them, and asserts that no Scala
 *  interpolation or helper-call leakage survives into the emitted
 *  Python source.
 */
class PyIRRuntimeContentTest:

  @Test def witnessAnchoredNames(): Unit =
    val c = PyIRRuntime.content
    assertTrue(c.contains("getName__Ljava_dlang_dString"))
    assertTrue(c.contains("toString__Ljava_dlang_dString"))
    assertTrue(c.contains("hashCode__I"))
    assertTrue(c.contains("_scpy_mod_java_lang_Class_"))

  @Test def noInterpolationLeak(): Unit =
    val c = PyIRRuntime.content
    assertFalse("no leaked Scala interpolation: ${ ... }", c.contains("$" + "{"))
    assertFalse("no raw helper-call leakage: PyMethodName", c.contains("PyMethodName"))
    assertFalse("no raw helper-call leakage: PySimpleMethodName", c.contains("PySimpleMethodName"))
