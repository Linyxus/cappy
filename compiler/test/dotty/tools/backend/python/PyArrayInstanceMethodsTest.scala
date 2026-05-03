package dotty.tools.backend.python

import org.junit.Assert.*
import org.junit.Test

/** Tests for the JVM-shape instance methods on the runtime
 *  `_scpy_Array` wrapper class — Wave 6 item 01a.
 *
 *  Background. `_scpy_Array(list)` is the runtime representation of a
 *  Scala `Array[T]`. It inherits `list` for backing storage and carries
 *  `_scpy_class` for the JVM `Class` contract. Pre-Wave-6 it defined
 *  only `getClass__Ljava_dlang_dClass` and `clone__Ljava_dlang_dObject`;
 *  user code calling `arr.toString` / `arr.hashCode` / `arr.equals(_)`
 *  through the post-erasure `Object`/`AnyRef` surface (e.g. `tests/run/t5680.scala`)
 *  raised `AttributeError: '_scpy_Array' object has no attribute
 *  'toString__Ljava_dlang_dString'`.
 *
 *  Encoder note. `PyEncoding.specialMethodNameOf` rewrites Scala-source
 *  `arr.hashCode` (nullary) to `arr.__hash__()` and `arr.equals(x)`
 *  (where `x: Any` / `x: Object`) to `arr.__eq__(x)`. The dunder forms
 *  are therefore what the user-facing call sites actually invoke; the
 *  mangled forms (`hashCode__I`, `equals__Ljava_dlang_dObject__Z`) are
 *  pinned for parity with `_scpy_Char` and to handle any post-erasure
 *  bridge that bypasses the encoder rewrite. `toString` has no such
 *  rewrite, so `arr.toString` lowers directly to
 *  `arr.toString__Ljava_dlang_dString()`.
 *
 *  These tests pin the structural rule that the prelude declares each
 *  of those JVM-shape methods on the `_scpy_Array` body.
 *
 *  `length` is intentionally NOT a property on `_scpy_Array`: array
 *  length lowers through the IR `ArrayLength` unary op, which
 *  `PyIREmitter` renders as `_scpy_len(arr)` — and `len(_scpy_Array(...))`
 *  works because the wrapper inherits `list.__len__`.
 */
class PyArrayInstanceMethodsTest:

  /** Slice the body of `class _scpy_Array(list):` out of the prelude
   *  string. We slice up to the next top-level `def ` or `class ` line
   *  (no leading whitespace) — i.e. the next definition outside the
   *  class body.
   */
  private def preludeBody: String =
    val full = PyIRRuntime.content
    val classMarker = "class _scpy_Array(list):"
    val classStart = full.indexOf(classMarker)
    assertTrue(
      "Prelude must define `class _scpy_Array(list):`.",
      classStart >= 0
    )
    val afterHeader = classStart + classMarker.length
    val tail = full.substring(afterHeader)
    val rel = tail.indexOf("\ndef ")
    val relC = tail.indexOf("\nclass ")
    val cuts = List(rel, relC).filter(_ >= 0)
    val end = if cuts.isEmpty then tail.length else cuts.min
    tail.substring(0, end)

  /** `preludeBody` with `#` comment lines stripped, so docstrings that
   *  mention a hazard (e.g. `self.__hash__()`) don't trip
   *  contains-checks that are looking for actual code. */
  private def preludeBodyCodeOnly: String =
    preludeBody.linesIterator
      .filterNot(line => line.trim.startsWith("#"))
      .mkString("\n")

  // -----------------------------------------------------------------
  //  toString__Ljava_dlang_dString — direct dispatch from `arr.toString`.
  // -----------------------------------------------------------------

  @Test def arrayToStringMethodIsDefined(): Unit =
    val body = preludeBody
    assertTrue(
      "_scpy_Array must define `toString__Ljava_dlang_dString`.",
      body.contains("def toString__Ljava_dlang_dString(self):")
    )

  @Test def arrayToStringUsesDescriptorFormat(): Unit =
    val body = preludeBody
    // Must use the array descriptor (matches JVM `Object.toString` on an
    // array — `[Lscala.runtime.BoxedUnit;@1a2b3c`). `_scpy_descriptor_for_class`
    // already returns `[Lpkg.Class;` for an array class, which is what
    // `t5680.scala` slices on `';'`.
    assertTrue(
      "Array toString must derive the prefix from `_scpy_descriptor_for_class`.",
      body.contains("_scpy_descriptor_for_class(self._scpy_class)")
    )
    assertTrue(
      "Array toString must produce `<descriptor>@<hex hash>`.",
      body.contains("\"@\"")
    )
    assertTrue(
      "Array toString hash must be `id(self) & 0xFFFFFFFF` rendered hex.",
      body.contains("_builtins.id(self) & 0xFFFFFFFF")
        && body.contains("_builtins.format(_builtins.id(self) & 0xFFFFFFFF, \"x\")")
    )

  @Test def arrayToStringDoesNotDispatchThroughDunderHash(): Unit =
    // `list.__hash__ is None` (lists are unhashable). The `_scpy_Object`
    // version of `toString` does `self.__hash__()` — that would TypeError
    // on `_scpy_Array`. Pin the rule that `toString` does NOT call
    // `self.__hash__()` (we use `_builtins.id(self)` directly so there
    // is no dependency on the `__hash__` slot's well-typedness).
    //
    // We restrict the search to the `toString` definition itself (not
    // the whole class body) — the trampoline `hashCode__I` legitimately
    // calls `self.__hash__()` to share its body with the dunder slot.
    val toStringDef = sliceDef("toString__Ljava_dlang_dString")
    val stripped = toStringDef.linesIterator
      .filterNot(line => line.trim.startsWith("#"))
      .mkString("\n")
    assertFalse(
      "_scpy_Array.toString must not call `self.__hash__()` — `list.__hash__` "
        + "is None and the call would TypeError.",
      stripped.contains("self.__hash__()")
    )

  /** Slice the body of a single method definition out of the
   *  `_scpy_Array` class body. Returns from `def <name>(` up to the
   *  next `def ` line (at the same indentation level) or the end of
   *  the class body. */
  private def sliceDef(name: String): String =
    val body = preludeBody
    val start = body.indexOf(s"def $name(")
    assertTrue(s"`def $name(` not found in _scpy_Array body", start >= 0)
    val tail = body.substring(start)
    // Find the next `\n    def ` (4-space indent — the same level as
    // the start `def `). The class body uses 4-space indentation, and
    // every method definition starts at exactly 4 spaces.
    val nextDef = tail.indexOf("\n    def ", 1)
    if nextDef < 0 then tail else tail.substring(0, nextDef)

  // -----------------------------------------------------------------
  //  hashCode — both `__hash__` (encoder rewrite target) and the
  //  mangled `hashCode__I` (parity / bridge fallback).
  // -----------------------------------------------------------------

  @Test def arrayDunderHashIsDefined(): Unit =
    // `arr.hashCode` lowers (per `PyEncoding.specialMethodNameOf`) to
    // `arr.__hash__()`. `list.__hash__ is None` so we MUST override.
    val body = preludeBody
    assertTrue(
      "_scpy_Array must define `__hash__` so `arr.hashCode` doesn't TypeError.",
      body.contains("def __hash__(self):")
    )

  @Test def arrayMangledHashCodeIsDefined(): Unit =
    // Defensive: any post-erasure bridge call that bypasses the
    // `__hash__` rewrite and emits the mangled name still finds a
    // method. Mirrors the dual `__hash__` + `hashCode__I` shape on
    // `_scpy_Char` (line ~2179, ~2186 of PyIRRuntime).
    val body = preludeBody
    assertTrue(
      "_scpy_Array must define mangled `hashCode__I` for parity with `_scpy_Char`.",
      body.contains("def hashCode__I(self):")
    )

  @Test def arrayHashCodeReturnsSigned32BitIdentityHash(): Unit =
    val body = preludeBody
    // Must mask to 32 bits and convert to signed (subtract 2**32 if the
    // top bit is set), per JVM `int` contract.
    assertTrue(
      "hashCode must mask `id(self)` to 32 bits.",
      body.contains("_builtins.id(self) & 0xFFFFFFFF")
    )
    assertTrue(
      "hashCode must coerce the 32-bit hash to signed (subtract 2**32 when high bit set).",
      body.contains("0x80000000") && body.contains("0x100000000")
    )

  // -----------------------------------------------------------------
  //  equals — both `__eq__` (encoder rewrite target) and mangled
  //  `equals__Ljava_dlang_dObject__Z` (bridge fallback).
  // -----------------------------------------------------------------

  @Test def arrayDunderEqIsDefined(): Unit =
    // `arr.equals(x)` (where `x: Any` / `x: Object`) lowers to
    // `arr.__eq__(x)`. Without our override, `_scpy_Array` would
    // inherit `list.__eq__` (structural list equality), which is
    // wrong for the JVM identity-equality contract on `Object`.
    val body = preludeBody
    assertTrue(
      "_scpy_Array must define `__eq__` to override structural `list.__eq__`.",
      body.contains("def __eq__(self, other):")
    )

  @Test def arrayDunderNeIsDefined(): Unit =
    // Symmetric override: `__ne__` mirrors the identity contract.
    // Without it, `list.__ne__` would invoke the structural compare.
    val body = preludeBody
    assertTrue(
      "_scpy_Array should define `__ne__` matching the identity `__eq__`.",
      body.contains("def __ne__(self, other):")
    )

  @Test def arrayMangledEqualsIsDefined(): Unit =
    // Defensive parity (see `arrayMangledHashCodeIsDefined`).
    val body = preludeBody
    assertTrue(
      "_scpy_Array must define mangled `equals__Ljava_dlang_dObject__Z` "
        + "for parity with `_scpy_Char`.",
      body.contains("def equals__Ljava_dlang_dObject__Z(self, other):")
    )

  @Test def arrayEqualsIsReferenceIdentity(): Unit =
    // All three equals shapes (`__eq__`, `__ne__`, mangled) must be
    // identity equality, not structural list compare.
    val body = preludeBody
    assertTrue(
      "_scpy_Array.equals must be reference identity (`self is other`), matching "
        + "the default JVM `Object.equals` contract.",
      body.contains("return self is other")
    )
    assertTrue(
      "_scpy_Array.__ne__ must be reference disequality (`self is not other`).",
      body.contains("return self is not other")
    )

  // -----------------------------------------------------------------
  //  length: NOT a property — already covered by `len(arr)`.
  // -----------------------------------------------------------------

  @Test def arrayLengthIsNotAddedAsAProperty(): Unit =
    // Verified in `GenPython.scala` / `PyIREmitter.scala`: `arr.length`
    // lowers to `PyUnaryCode.ArrayLength`, which emits `_scpy_len(arr)`.
    // `_scpy_len` is `_builtins.len` (line ~621 of PyIRRuntime), and
    // `len()` on a `list`-subclass wrapper just works.
    val body = preludeBody
    assertFalse(
      "_scpy_Array must NOT carry a `length` property — IR `ArrayLength` "
        + "lowers to `_scpy_len(arr)`, which already works on a `list`-subclass.",
      body.contains("def length") || body.contains("@property")
    )

  // -----------------------------------------------------------------
  //  Sanity: pre-existing methods are still there.
  // -----------------------------------------------------------------

  @Test def existingArrayMethodsAreRetained(): Unit =
    val body = preludeBody
    assertTrue(body.contains("def getClass__Ljava_dlang_dClass(self):"))
    assertTrue(body.contains("def clone__Ljava_dlang_dObject(self):"))

end PyArrayInstanceMethodsTest
