# Layer 6 — AssertionError cluster diagnosis

Read-only investigation by sub-agent a7-assertion-investigate (2026-05-01)
of the 18 fixtures whose primary failure is a Scala-level `assert(...)` /
`Assert.assertX` failing at runtime under the Python backend.

These fixtures pass on the JVM but fail under the Python backend — meaning
the bugs live in codegen, the runtime, or pylib stdlib ports. There is **no
single root cause**; this note breaks them into 6 categories.

## Per-fixture findings

### break-opt.scala
- **Failed assert location**: `tests/run/break-opt.scala:87` (and others, lines 88–101)
- **Root cause category**: Control flow — `boundary` / `break` semantics
- **Suspect codegen site**: line 2229 in generated `.py` shows `(lambda: (_ for _ in ()).throw(_scpy_ex))()` as the break mechanism
- **Hypothesis**: The `boundary` label and `break` continuation mechanism does not preserve non-local return semantics when `break` should exit nested loops. Generator-throw doesn't propagate to the correct label frame.

### breaks.scala
- **Failed assert location**: `tests/run/breaks.scala:29` (first assertion in main)
- **Root cause category**: Control flow — `boundary` / `break` labelled return
- **Suspect codegen site**: line 2327 — same generator-throw pattern
- **Hypothesis**: Same root cause as `break-opt`. `boundary` requires a Python-native control-flow primitive (label-keyed exceptions) that the codegen isn't currently emitting.

### equality.scala
- **Failed assert location**: `tests/run/equality.scala:18` (hash equality across nested loops)
- **Root cause category**: hashCode/equals — numeric type coercion in hash
- **Suspect codegen site**: line 2272 calls `_scpy_unbox_or_default` while computing hash
- **Hypothesis**: When comparing `x == y` and `hash(x) == hash(y)` across numeric types (Int/Long/Float/Double/BigInt/BigDecimal), `##` must return the same hash for numerically-equal values across type boundaries (`5 == 5L == 5.0 == 5f`). The codegen isn't implementing cross-type numeric hash equivalence.

### getclass.scala
- **Failed assert location**: `tests/run/getclass.scala:44–45` (lambda class identity)
- **Root cause category**: getClass / class identity — boxed lambda Class
- **Suspect codegen site**: line 2336
- **Hypothesis**: `getClass()` on lambda objects must return a stable Class for the synthesized lambda. Python function closures may not have stable class identity, or the `Function*` wrappers aren't assigning a proper `__class__`.

### hashCodeDistribution.scala
- **Failed assert location**: `tests/run/hashCodeDistribution.scala:14` (collision rate on case-class hashCode)
- **Root cause category**: hashCode/equals — case-class hash distribution
- **Suspect codegen site**: line 2474
- **Hypothesis**: Case-class `hashCode` is not being generated with proper field-mixing (Murmur3). Probably falls back to Python's default object hash, giving poor distribution.

### hashhash.scala
- **Failed assert location**: `tests/run/hashhash.scala:5` (`##` differs from `.hashCode` on floats)
- **Root cause category**: hashCode/equals — `##` (structured hash) vs `.hashCode()` for floats
- **Suspect codegen site**: line 2249
- **Hypothesis**: `##` (`scala.runtime.Statics.mix/hashCode`) must produce different hashes than `.hashCode()` for Float/Double because `##` is structure-preserving and notices `5.0f != 1.0d` (different bits), whereas `.hashCode()` on boxed numbers converges. The codegen must distinguish.

### i10527.scala
- **Failed assert location**: `tests/run/i10527.scala:6` (`canEqual` override on case class)
- **Root cause category**: hashCode/equals — `canEqual` not respected in generated `__eq__`
- **Suspect codegen site**: line 2432
- **Hypothesis**: `assert(C(1) != new CC(1))` requires the generated equality to consult `canEqual` before comparing fields; it likely isn't.

### i12976.scala
- **Failed assert location**: `tests/run/i12976.scala:35` (NPE expected from generic null)
- **Root cause category**: Numeric coercion / null handling — NPE from generic type
- **Suspect codegen site**: line 2485
- **Hypothesis**: Test expects `b.b2(s)` (generic returning `null.asInstanceOf[X]`) to throw NPE; `assert(false)` should not be reached. Either null isn't propagating, or the assert is reached when it shouldn't be.

### i19224.scala
- **Failed assert location**: `tests/run/i19224.scala:9` (default-param self-reference at module init)
- **Root cause category**: Lazy val / module initialization — field access before init
- **Suspect codegen site**: line 2302 during module construction
- **Hypothesis**: `NoSourcePosition` extends `SourcePosition` with a default param that references itself. `assert(NoSourcePosition.outer == null)` expects the cycle to resolve to null; the codegen reads `.outer` before init completes.

### i4659b.scala
- **Failed assert location**: `tests/run/i4659b.scala:9` (lambda equality)
- **Root cause category**: getClass / class identity — lambda memoization & equality
- **Suspect codegen site**: line 2535
- **Hypothesis**: `assert(x() == x())` where `x()` returns a lambda. JVM memoizes static lambdas; Python is creating a new function each call, so identity differs.

### i6710.scala
- **Failed assert location**: `tests/run/i6710.scala:4` (NaN comparison)
- **Root cause category**: Numeric coercion — NaN
- **Suspect codegen site**: line 2412
- **Hypothesis**: `Float.NaN > 0.0f` must be false. Python's bare comparison handles this, but `_scpy_unbox_or_default` may be losing NaN through intermediate conversions.

### i8314.scala
- **Failed assert location**: `tests/run/i8314.scala:8` (tuple equality with cross-type numerics)
- **Root cause category**: hashCode/equals — tuple equality with numeric coercion
- **Suspect codegen site**: line 2276
- **Hypothesis**: `assert((1, 2) == (1, 2.0))` requires numeric weak conformance in tuple `__eq__`.

### is-valid-num.scala
- **Failed assert location**: `tests/run/is-valid-num.scala:32` (`BigDecimal.isWhole` on `0.1`)
- **Root cause category**: Numeric coercion — BigDecimal method semantics
- **Suspect codegen site**: line 2297, `self.y1__Lscala_dmath_dBigDecimal()`
- **Hypothesis**: `BigDecimal("0.1").isWhole` must be false. The pylib BigDecimal port either lacks `isWhole`, or its conversion between Python `Decimal` and Scala `BigDecimal` is wrong.

### loops.scala
- **Failed assert location**: `tests/run/loops.scala:33` (boundary/break in while)
- **Root cause category**: Control flow — `boundary` / `break` in while loops
- **Suspect codegen site**: line 2256
- **Hypothesis**: Same root cause as `break-opt`/`breaks`. The inline `loop` macro applies labels that the Python backend isn't preserving.

### properties-version-string.scala
- **Failed assert location**: `tests/run/properties-version-string.scala:5`
- **Root cause category**: String / runtime — `scala.util.Properties.versionNumberString`
- **Suspect codegen site**: line 2226
- **Hypothesis**: `Properties.versionNumberString` is unimplemented in pylib (or returns empty). The assert `v.nonEmpty && v.startsWith("3.")` then fails.

### t13033.scala
- **Failed assert location**: `tests/run/t13033.scala:42` (case-class hashCode matches reference Murmur3)
- **Root cause category**: hashCode/equals — case-class hash matches reference
- **Suspect codegen site**: line 3149 during init
- **Hypothesis**: `assert(c1.hashCode == caseClassHash(c1))` requires the generated `hashCode` to match Murmur3 mixing exactly, including productPrefix mixing.

### t7912.scala
- **Failed assert location**: `tests/run/t7912.scala:12` (MatchError message)
- **Root cause category**: String/format — exception message generation
- **Suspect codegen site**: line 2404
- **Hypothesis**: `me.getMessage() == "an instance of class A$"`. The Python `MatchError` ctor or format string is producing a different message.

### weak-conformance.scala
- **Failed assert location**: `tests/run/weak-conformance.scala:17`
- **Root cause category**: Numeric coercion — weak conformance in collection
- **Suspect codegen site**: line 2304
- **Hypothesis**: `List(1.0f, 0)` with expected `List[AnyVal]` requires weak conformance widening of `0` to `0.0f`. The codegen for collection construction isn't applying weak conformance.

## Synthesis: 6 categories, 18 fixtures

| Category | Fixtures | Count | Fix scope |
|---|---|---:|---|
| Control flow (`boundary`/`break`) | break-opt, breaks, loops | 3 | Redesign label/exception mechanism for non-local returns |
| Numeric hash/equals coercion | equality, hashCodeDistribution, hashhash, i10527, i8314 | 5 | Implement Murmur3 + cross-type numeric hashing; respect `canEqual` |
| Lazy val / module init | i19224, i4659b | 2 | Layer 5.7 follow-up (early-init / cache lambda Class) |
| Numeric type special cases | i6710, is-valid-num | 2 | NaN unboxing; BigDecimal methods in pylib |
| Class identity / lambda caching | getclass | 1 | Stable lambda Class objects (`__class__` assignment) |
| String / runtime | properties-version-string, t7912 | 2 | `scala.util.Properties` impl; MatchError message |

The top three categories cover **10 of 18** fixtures. The remaining 8 are
smaller, more surgical fixes.

## Recommended order

1. **Layer 5.7 follow-up** (already in flight as agent a6): may pick up
   i19224 and i4659b transitively.
2. **Numeric hash/equals coercion** — single PR implementing Murmur3 +
   cross-type hash equivalence + `canEqual` check would flip 5 fixtures.
3. **Control flow `boundary`/`break`** — needs a small label-keyed
   exception runtime; flips 3 fixtures.
4. **String/format and BigDecimal/NaN** — small individual fixes, can be
   bundled.
