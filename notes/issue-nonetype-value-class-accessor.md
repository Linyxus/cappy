# Layer 5.1 — fixture cohort split into separate sub-issues

The original umbrella issue ("value-class accessor on `None` raises
`AttributeError`") turned out to be a misdiagnosis. The five fixtures listed
in `notes/pyrun-fix-roadmap.md` Layer 5.1 hit five DIFFERENT bugs once
investigated. Notes by fixture below.

## Status table

| Fixture | Real symptom | Root cause | Fix |
|---|---|---|---|
| `tests/run/t7396.scala` | `AssertionError` (was thought to be NoneType.x__I) | `_scpy_Object.toString` used identity hash instead of dispatching `hashCode()`; value classes override `__hash__`, so toString printed `L@a063c50` instead of `L@0` | **Landed**: `PyIRRuntime.scala` toString now calls `self.__hash__()` |
| `tests/run/exceptions-2.scala` | `AttributeError: 'NoneType' object has no attribute 'x__I'` | Source does `val a: Leaf = null; println(a.x)` and expects `NullPointerException` to be caught. `Leaf` is a regular case class, not a value class. Python `AttributeError` is not translated to `NullPointerException`. | Deferred to **5.1.a** below |
| `tests/run/lambda-null.scala` | `AssertionError` | `genericCall1(if1_specialized)` calls `apply(null)` → specialized lambda gets Python `None`, prints `null`, returns `None` instead of unboxing to `0`. Function specialization bridge does not unbox null arguments. | Deferred to **5.1.b** below |
| `tests/run/numbereq.scala` | `AttributeError: 'NoneType' object has no attribute 'bigDecimal__Ljava_dmath_dBigDecimal'` | `BigDecimal.__eq__` was emitted from `equals(BigDecimal)` overload (which calls `compare(that)`) rather than `equals(Any)` overload (which pattern-matches first). Both encode to `__eq__` and the second emission wins. | Deferred to **5.1.c** below |
| `tests/run/t4122.scala` | `AssertionError` | `Seq[Char].##` differs across `String`, `IndexedSeq` from Array, `Seq.apply`, and `String.toList`. Seq hashCode bug, unrelated to value classes. | Deferred to **5.1.d** below |

## What landed

**Fix to `_scpy_Object.toString__Ljava_dlang_dString`** in
`compiler/src/dotty/tools/backend/python/PyIRRuntime.scala`. Replaced
`_scpy_identity_hash_code(self)` with `self.__hash__() & 0xFFFFFFFF`.

Rationale: JVM `Object.toString` is `getClass().getName() + "@" +
Integer.toHexString(hashCode())`, where `hashCode()` is virtual dispatch.
Value classes override `__hash__` (mapped from Scala `hashCode`), so calling
`self.__hash__()` picks up the wrapped-value hash. The `& 0xFFFFFFFF` mimics
`Integer.toHexString` on a negative int: Python `format(-1, 'x') == '-1'`
but Java prints `'ffffffff'`.

`_scpy_identity_hash_code` is still used by `System.identityHashCode` and
by emitter fallback `__hash__` bodies (no override → identity-like default).

Verification:
- `tests/run/t7396.scala` now exits 0 (assertions pass).
- 73/73 unit tests + 215/215 pos-py + 3/3 negScalaPy regression suites: green.

## Sub-issues to file

### 5.1.a `AttributeError` from None-receiver method dispatch is not catchable as `NullPointerException`

Reproducer: `tests/run/exceptions-2.scala` `Test.method2`:
```scala
val a: Leaf = null
println(a.x)
// catches `case _: NullPointerException`
```
`Leaf` is a case class (NOT a value class). Generated code is
`a_2.x__I()` with `a_2 = None`, raising `AttributeError`. The catch arm
tests `_scpy_is_value_of_type(ex, _scpy_class_of_name("java.lang.NullPointerException"))`,
which is `False` for `AttributeError`.

Possible fixes (each has tradeoffs):
- (i) In `_scpy_is_value_of_type`, treat `AttributeError` as
  `NullPointerException`. Risk: real Python AttributeErrors (from
  facades/dynamic) would also be rewritten. Could narrow with
  message-pattern check ("'NoneType' object").
- (ii) In codegen, wrap every instance-method dispatch in a try/except that
  converts `AttributeError` on `None` receiver to `NullPointerException`.
  Heavy.
- (iii) Lower null-receiver checks at compile time when the receiver type
  is statically nullable. Best long-term but bigger.

Probably (i) with a narrow `NoneType` filter is cheapest.

### 5.1.b Function specialization bridge does not unbox `null` arguments

Reproducer: `tests/run/lambda-null.scala` `genericCall1[A,B](foo: A=>B) =
foo(null.asInstanceOf[A])`. When `foo` is `Int=>Int` (specialized), the
generic `apply(Object): Object` bridge passes `null` straight through to
the specialized `apply$mcII$sp(int): int` body, which then prints
`specialized Function1: null` and returns `None`. Scala JVM unboxes
`null → 0` in the bridge.

Fix probably belongs in `_scpy_Fn1` / `Function1` runtime in
`PyIRRuntime.scala` or wherever `apply_mcII_sp` forwards from
`apply__Ljava_dlang_dObject__Ljava_dlang_dObject`. Need to unbox `None`
to the primitive default per the result type tag.

### 5.1.c Overloaded `equals` collapses both onto `__eq__` (last writer wins)

Reproducer: `tests/run/numbereq.scala` exercises
`scala.math.BigDecimal.equals`. Source has both
`equals(that: Any): Boolean` and `equals(that: BigDecimal): Boolean` (two
distinct methods). `PyEncoding.specialMethodNameOf` maps every 1-arg
`equals` to `__eq__`. Generated Python keeps only the typed-overload
version (`return compare(that) == 0`), which then dereferences `that`
without a type guard and explodes when `that = None` (e.g. comparing
BigDecimal to a non-Number).

Fix: only map `equals(Any)` to `__eq__`. The typed overload should keep
its mangled name (`equals_extension__Lscala_dmath_dBigDecimal__Z` or
similar). Same logic applies to any value-class `equals(SpecificType)`.

### 5.1.d `Seq[Char]` hashCode disagreement across collection backings

Reproducer: `tests/run/t4122.scala`. `"ab".##`, `Array('a','b').toIndexedSeq.##`,
`Seq('a','b').##`, `"ab".toList.##` should all be equal (Scala `Seq.hashCode`
contract). On Python they differ. Probably caused by `WrappedString`,
`ArraySeq`, `List.hashCode` walking different paths. Investigate
`MurmurHash3` / `Statics.unorderedHash`/`orderedHash` ports.

## Original umbrella note (preserved for context)

[See git history of this file before the Layer 5.1 split for the
"value-class accessor on None" hypothesis. Discarded — none of the five
fixtures actually share that root cause.]
