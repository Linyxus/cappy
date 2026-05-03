package dotty.tools.backend.python

import org.junit.Assert.*
import org.junit.Test

/** Tests for the boxed-primitive instance-method dispatch helpers — Wave
 *  6 item 01b. Pins the structural shape of `_scpy_Double_*` /
 *  `_scpy_Boolean_*` runtime helpers in the prelude (`PyIRRuntime.content`)
 *  and the corresponding `virtualCallSeeds` entries.
 *
 *  Background. Boxing is identity on this backend: `boxToDouble(d)` (and
 *  the `Predef.double2Double` implicit) returns the same Python `float`.
 *  When user code calls a JVM-shape instance method through an `Object`/
 *  `AnyRef`/`java.lang.Number`/boxed-primitive static receiver, the
 *  receiver carries no encoded `isNaN__Z` / `intValue__I` etc. attribute,
 *  and the call `AttributeError`s. `GenPython.genBoxedPrimitiveCall`
 *  rewrites such calls into `_scpy_Double_<method>` / `_scpy_Boolean_*`
 *  helpers in the prelude that dispatch on `isinstance(x, float|int|bool)`.
 *
 *  These tests exercise the prelude text (helper functions exist, dispatch
 *  on `isinstance`, fall through on non-primitive). Behavioural correctness
 *  on the codegen-intercept side is exercised end-to-end by
 *  `tests/run/blame_eye_triple_eee-{double,float}.scala` (formerly
 *  `nan-instance-method-dispatch` excludelist tags, removed by item 01b).
 */
class PyBoxedPrimitiveDispatchTest:

  private def prelude: String = PyIRRuntime.content

  /** Slice from `def <name>(` up to the next top-level `def ` line so we
   *  can assert the body shape of one specific helper without picking up
   *  text from neighboring helpers.
   */
  private def sliceTopLevelDef(name: String): String =
    val full = prelude
    val marker = s"def $name("
    val start = full.indexOf(marker)
    assertTrue(s"prelude must define top-level `def $name(`", start >= 0)
    val tail = full.substring(start)
    // Next top-level def starts at column 0 with `def `.
    val nextDef = tail.indexOf("\ndef ", 1)
    val nextCls = tail.indexOf("\nclass ", 1)
    val cuts = List(nextDef, nextCls).filter(_ >= 0)
    if cuts.isEmpty then tail else tail.substring(0, cuts.min)

  // -----------------------------------------------------------------
  //  isNaN — the Wave-6-item-01b named symptom on raw `float`.
  // -----------------------------------------------------------------

  @Test def isNaNHelperIsDefined(): Unit =
    assertTrue(
      "prelude must define `_scpy_Double_isNaN`",
      prelude.contains("def _scpy_Double_isNaN(x):")
    )

  @Test def isNaNDispatchesOnFloat(): Unit =
    val body = sliceTopLevelDef("_scpy_Double_isNaN")
    // Must isinstance-check `float` and call `_scpy_math.isnan` on it.
    assertTrue(
      "_scpy_Double_isNaN must isinstance-check on `float`",
      body.contains("isinstance(x, float)") || body.contains("isinstance(x, (int, float))")
    )
    assertTrue(
      "_scpy_Double_isNaN must use `_scpy_math.isnan` for the float branch",
      body.contains("_scpy_math.isnan(x)")
    )

  @Test def isNaNFallsThroughToEncodedMethod(): Unit =
    // For real ported boxed-Double instances, the helper must defer to
    // the encoded virtual method so `virtualCallSeeds` entry is non-dead
    // on the runtime side too.
    val body = sliceTopLevelDef("_scpy_Double_isNaN")
    assertTrue(
      "_scpy_Double_isNaN must fall through to `x.isNaN__Z()` for non-primitive receivers",
      body.contains("x.isNaN__Z()")
    )

  @Test def isNaNHandlesBoolBeforeInt(): Unit =
    // `bool` is a `int` subclass in Python — the bool short-circuit must
    // come *before* any int-or-float dispatch, otherwise `isinstance(True,
    // (int, float))` matches first and returns True/False through the
    // wrong arm.
    val body = sliceTopLevelDef("_scpy_Double_isNaN")
    val boolIdx = body.indexOf("isinstance(x, bool)")
    val intFloatIdx = body.indexOf("isinstance(x, (int, float))")
    assertTrue("`isinstance(x, bool)` must come before the int/float dispatch", boolIdx >= 0)
    if intFloatIdx >= 0 then
      assertTrue(
        "bool short-circuit must precede the int/float dispatch",
        boolIdx < intFloatIdx
      )

  // -----------------------------------------------------------------
  //  Numeric *Value: at least one cross-type unboxing helper.
  // -----------------------------------------------------------------

  @Test def doubleValueHelperIsDefined(): Unit =
    assertTrue(
      "prelude must define `_scpy_Double_doubleValue`",
      prelude.contains("def _scpy_Double_doubleValue(x):")
    )

  @Test def intValueHelperIsDefined(): Unit =
    assertTrue(
      "prelude must define `_scpy_Double_intValue`",
      prelude.contains("def _scpy_Double_intValue(x):")
    )

  @Test def intValueClampsFloatOverflow(): Unit =
    // JVM `(int)Double.POSITIVE_INFINITY == Integer.MAX_VALUE`; the
    // helper must not naively `int(Inf)` (raises OverflowError in Python).
    val body = sliceTopLevelDef("_scpy_Double_intValue")
    assertTrue(
      "_scpy_Double_intValue must clamp positive-infinity to `Int.MaxValue`",
      body.contains("2147483647")
    )
    assertTrue(
      "_scpy_Double_intValue must clamp negative-infinity to `Int.MinValue`",
      body.contains("-2147483648")
    )
    assertTrue(
      "_scpy_Double_intValue must short-circuit NaN to 0",
      body.contains("isnan(x)")
    )

  @Test def doubleValueIsIdentityOnFloat(): Unit =
    val body = sliceTopLevelDef("_scpy_Double_doubleValue")
    assertTrue(
      "_scpy_Double_doubleValue must coerce raw int/float through `_builtins.float`",
      body.contains("_builtins.float(x)")
    )

  // -----------------------------------------------------------------
  //  Boolean: minimal helper.
  // -----------------------------------------------------------------

  @Test def booleanValueHelperIsDefined(): Unit =
    assertTrue(
      "prelude must define `_scpy_Boolean_booleanValue`",
      prelude.contains("def _scpy_Boolean_booleanValue(x):")
    )

  @Test def booleanValueIsIdentityOnBool(): Unit =
    val body = sliceTopLevelDef("_scpy_Boolean_booleanValue")
    assertTrue(
      "_scpy_Boolean_booleanValue must short-circuit `isinstance(x, bool)` to `x`",
      body.contains("isinstance(x, bool)") && body.contains("return x")
    )
    assertTrue(
      "_scpy_Boolean_booleanValue must fall through to `x.booleanValue__Z()`",
      body.contains("x.booleanValue__Z()")
    )

  // -----------------------------------------------------------------
  //  Reachability seeds.
  // -----------------------------------------------------------------

  @Test def virtualCallSeedsIncludeBoxedPrimitiveMethods(): Unit =
    // The `_scpy_Double_*` helpers fall through to the encoded virtual
    // methods on real ported boxes. Without `virtualCallSeeds` entries,
    // DCE would prune those subtype overrides and the helper's fallback
    // `AttributeError`s.
    val seeds = PyIRRuntime.virtualCallSeeds
    val ownersAndNames = seeds.map { case (owner, m) => (owner.nameString, m.simple.name) }
    assertTrue(
      s"virtualCallSeeds must include `java.lang.Double#isNaN`, got $ownersAndNames",
      ownersAndNames.contains(("java.lang.Double", "isNaN"))
    )
    assertTrue(
      s"virtualCallSeeds must include `java.lang.Double#doubleValue`, got $ownersAndNames",
      ownersAndNames.contains(("java.lang.Double", "doubleValue"))
    )
    assertTrue(
      s"virtualCallSeeds must include `java.lang.Boolean#booleanValue`, got $ownersAndNames",
      ownersAndNames.contains(("java.lang.Boolean", "booleanValue"))
    )

end PyBoxedPrimitiveDispatchTest
