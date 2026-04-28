# `Object.getClass()` on Python primitives raises `AttributeError`

## Minimal example for reproducing

```scala
@main def Test =
  val x: Any = 1
  println(x.getClass)   // expected: class java.lang.Integer
  val s: Any = "hi"
  println(s.getClass)   // expected: class java.lang.String
```

Real fixtures hitting this: `tests/run/matchable.scala`,
`tests/run/string-switch.scala`.

## Output vs Expected Behaviour

```
AttributeError: 'int' object has no attribute 'getClass__Ljava_dlang_dClass'
AttributeError: 'str' object has no attribute 'getClass__Ljava_dlang_dClass'
```

Expected: prints `class java.lang.Integer` (or whatever the boxed form should
report).

## Quick Analysis

`x.getClass()` lowers to a direct method call
`x.getClass__Ljava_dlang_dClass()` in generated Python. That works for
user-defined classes (which inherit from `Object` and have the method
synthesized by the runtime), but fails for Python primitives `int`, `str`,
`float`, `bool`, etc., which are passed through unboxed and don't have any
method namespace.

There is no current runtime helper for "give me the Class object for an
arbitrary value, including primitives." The runtime contract in
`compiler/src/dotty/tools/backend/python/PyIRRuntime.scala` could grow one.

**Fix shape**:

1. Add `_scpy_get_class(obj)` to the Python prelude (PyIRRuntime). It dispatches:
   - if `isinstance(obj, int)` → return the Class object for `java.lang.Integer`
   - if `isinstance(obj, str)` → `java.lang.String`
   - if `isinstance(obj, float)` → `java.lang.Double`
   - else fall back to `obj.getClass__Ljava_dlang_dClass()`.
2. Codegen for `Object.getClass` (in GenPython / PyEncoding) lowers to a call
   to `_scpy_get_class(obj)` instead of a direct method call when the
   receiver's static type is `Any` / `Object` / `Matchable` (i.e. not provably
   non-primitive).

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D3).
