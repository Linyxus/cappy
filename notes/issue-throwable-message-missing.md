# `Throwable.getMessage()` raises `AttributeError: 'X' has no attribute 'message'`

## Minimal example for reproducing

```scala
@main def Test =
  try throw new ArithmeticException("boom")
  catch case e: ArithmeticException => println(e.getMessage)
```

Affected fixtures: `tests/run/fully-abstract-nat-2.scala`,
`tests/run/i11050.scala`.

## Output vs Expected Behaviour

```
AttributeError: 'ArithmeticException' object has no attribute 'message'
```

Expected: prints `boom`.

## Quick Analysis

`getMessage()` lowers to a Python attribute read of `self.message`, but the
generated Python class for `ArithmeticException` (and presumably any other
`java.lang.Throwable` subclass routed through pylib's
`pylib-py/src/java/lang/Throwable.scala`) does not initialize a `message`
instance attribute.

Two possible roots:

1. The pylib `Throwable` class declares `message` as `private` but the
   `getMessage` accessor isn't wired to actually read it as `self.message`
   under the encoded name (e.g. needs `_message` mangling or the getter body
   reads the wrong slot).
2. The constructor `Throwable(String)` doesn't store the argument into
   `self.message` at all on the Python side — codegen for the param-storing
   constructor path fails for `Throwable`-rooted hierarchies.

Inspect `pylib-py/src/java/lang/Throwable.scala` and the generated
`out/.../<fixture>.py` around the `__init__` / `getMessage` of
`ArithmeticException` to confirm which.

**Fix shape**: ensure `Throwable.__init__(message)` stores the message into a
field accessed by `getMessage`, under the encoded field name PyEncoding emits
for the constructor parameter. Probably a single-line fix once the right slot
is identified.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D2).
