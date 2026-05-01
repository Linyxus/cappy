# Primitive `Array[Long]` element loses its boxed-type tag

Surfaced 2026-05-02 after fixing Bug A (isArray infinite recursion) in
`notes/issue-stockrun-timeout-cluster.md`. Was previously masked by the
infinite loop in `ScalaRunTime.isArray`.

## Minimal example for reproducing

```scala
object Test:
  def main(args: Array[String]): Unit =
    val a: Array[Long] = Array(1L)
    val any: Any = a(0)
    println(any.isInstanceOf[Long])             // false (expected: true)
    println(any.isInstanceOf[java.lang.Long])   // false (expected: true)
    println(any.isInstanceOf[java.lang.Integer]) // true  (wrong)
    println(any.getClass.getName)                // java.lang.Integer (wrong)
```

Affected fixture: `tests/run/i14693.scala`. Likely-affected pattern set:
`case Array(i: Long)`, `case Array(i: Byte)`, `case Array(i: Short)` for
their respective primitive backing arrays.

## Quick analysis

`_scpy_Array.__getitem__` only special-cases `char[]` reads to box raw
ints into `_scpy_Char` (`PyIRRuntime.scala:1248-1252`). Reads from
primitive `long[]` / `byte[]` / `short[]` / `int[]` return a raw Python
`int` whose `_scpy_class_of_instance` resolves to `java.lang.Integer`
unconditionally.

On the JVM, `(a: Array[Long])(0)` yields a `Long`-typed boxed value
(via autoboxing), so `isInstanceOf[Long]` succeeds. We need the same
preservation here: when the element flows into an `Any` slot or a
type-test, the runtime needs to know its declared component type was
`long`, not `int`.

## Fix shape

Extend the existing `_scpy_is_char_array` path in `_scpy_Array.__init__`
to track the component primitive name, and `__getitem__` to box reads
from `long[]` / `byte[]` / `short[]` arrays into a thin int subclass
whose class registers as `java.lang.Long` / `Byte` / `Short` (so
`getClass`, `_scpy_is_value_of_type`, and `isInstanceOf` all line up).
`int[]` and `float[]`/`double[]` may not need wrappers if the matching
test is against `Int`/`Float`/`Double` — verify against the affected
fixture set before deciding.

## Out of scope

- The Bug A fix (Option 2) is already landed. This is the strictly
  smaller residual that survived it.
- The `Array(i: Long)` extractor in user code is the canonical reaching
  call site. If the fix turns out to be wider than expected, gate it
  behind a primitive-component check in `_scpy_Array.__init__` so
  reference-typed arrays remain unaffected.
