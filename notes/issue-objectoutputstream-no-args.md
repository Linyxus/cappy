# `new ObjectOutputStream(out)` raises `TypeError: takes no arguments`

## Minimal example for reproducing

```scala
import java.io.{ByteArrayOutputStream, ObjectOutputStream}

@main def Test =
  val baos = new ByteArrayOutputStream()
  val oos = new ObjectOutputStream(baos)
  oos.writeInt(42)
  oos.close()
```

Affected fixtures: `tests/run/defaults-serizaliable-with-forwarders`,
`tests/run/enums-serialization-compat`, `tests/run/i16390`,
`tests/run/t3038d`, `tests/run/t5590` (5 fixtures).

## Output vs Expected Behaviour

```
TypeError: ObjectOutputStream() takes no arguments
```

Expected: serialization completes; `oos.close()` returns cleanly.

## Quick Analysis

The Python class for `java.io.ObjectOutputStream` in pylib has a no-argument
`__init__`, but every JVM caller passes an `OutputStream` argument. Either:

(a) `pylib-py/src/java/io/ObjectOutputStream.scala` is a stub that elided the
constructor — needs a real `(out: OutputStream)` constructor that stores the
argument and provides `writeInt`, `writeObject`, `writeUTF`, `flush`, `close`,
etc.

(b) The codegen for constructor invocations is calling the synthetic empty
constructor instead of the parameterized one. This would suggest a more
general constructor-dispatch issue in PyIREmitter that would also affect
other classes; worth checking by inspecting one fixture's generated `.py`.

(a) is the more likely culprit. Verify by reading
`pylib-py/src/java/io/ObjectOutputStream.scala` (if it exists) or
`ls pylib-py/src/java/io/` to confirm it's missing.

**Fix shape**: implement `ObjectOutputStream(out)` in pylib backed by a Python
`pickle` writer or a simple length-prefixed binary format. The fixtures only
exercise basic primitive/enum serialization, so a minimal subset is enough:
`writeInt`, `writeObject`, `writeBoolean`, `writeUTF`, `flush`, `close`.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D8).
