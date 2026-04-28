# Value-class field accessor on `None` raises `AttributeError`

## Minimal example for reproducing

```scala
class L(val x: Int) extends AnyVal

@main def Test =
  val l: L = new L(0)
  // Some path through the codegen produces a None where the accessor expects an L
  println(l.x)
```

(The actual fixtures hit this through more elaborate paths.) Affected fixtures:
`tests/run/exceptions-2.scala`, `tests/run/lambda-null.scala`,
`tests/run/numbereq.scala`, `tests/run/t4122.scala`. `tests/run/t7396.scala`
also fits this family (value class with `null` argument).

## Output vs Expected Behaviour

```
AttributeError: 'NoneType' object has no attribute 'x__I'
AttributeError: 'NoneType' object has no attribute 'bigDecimal__Ljava_dmath_dBigDecimal'
```

Expected: each fixture's assertions pass.

## Quick Analysis

Value classes are erased in the JVM backend so `l.x` is just an unboxed read.
On the Python side the value class survives as a real Python class and `.x` is
a method/attribute access on the wrapper. When the wrapper instance is `None`
(e.g. `new L(null)`-equivalent or an uninitialized field path), the accessor
fails because `None.x__I` is a `NoneType` attribute lookup.

The shared root may be one of:

(a) **Value-class boxing/unboxing mismatch**: codegen sometimes emits the
    erased form (read `obj.x`), sometimes the boxed form (call
    `obj.x__I()`). For `null` receivers, neither works without a guard.

(b) **Uninitialized field default**: an instance field of a value-class type
    is initialized to `None` (Python default) instead of the JVM-equivalent
    "default boxed wrapper", and the accessor walks into it.

(c) **`null.asInstanceOf[L]`** path: explicit casts of `null` to a value-class
    type produce a `None` instead of a wrapper. Subsequent accessor calls
    blow up.

Each of the four fixtures may hit a different sub-pattern. Inspect the
generated `.py` for one of them (e.g. `exceptions-2.py` around the Tree case
classes) and trace which sub-pattern applies. The fixture
`tests/run/t7396.scala` explicitly constructs `new L(0)` and `new M(null)`
and asserts on hashCode/toString — that's the most direct repro for the
value-class-with-null sub-pattern.

**Fix shape**: depends on sub-pattern. Likely a combination of:
- guard the value-class accessor codegen so it returns the boxed-default
  rather than calling through `None`;
- or unify the boxing strategy so the accessor is always the same form
  regardless of context.

Defer to per-fixture inspection. Specialist report:
`/tmp/pyrun-analysis/cat-d-attr-errors.md` (D1).
