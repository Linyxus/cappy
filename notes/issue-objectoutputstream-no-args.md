# Round-trip serialization needs `ObjectInputStream`

**Status:** write side **landed**; read side missing. `ObjectOutputStream(out)`
now exists in `pylib-py/src/java/io/ObjectOutputStream.scala`. The
serialization round-trip fixtures still fail because there is no companion
`ObjectInputStream` to read what the writer produced.

## Affected fixtures

`tests/run/defaults-serizaliable-with-forwarders`,
`tests/run/enums-serialization-compat`, `tests/run/i16390`,
`tests/run/t3038d`, `tests/run/t5590` — all do a write/read round-trip and
fail at the read side.

## Minimal example

```scala
import java.io.{
  ByteArrayInputStream, ByteArrayOutputStream,
  ObjectInputStream, ObjectOutputStream
}

@main def Test =
  val baos = new ByteArrayOutputStream()
  val oos = new ObjectOutputStream(baos)
  oos.writeInt(42)
  oos.close()

  val bais = new ByteArrayInputStream(baos.toByteArray)
  val ois = new ObjectInputStream(bais)  // missing in pylib
  println(ois.readInt())                 // expect: 42
```

## Fix shape

Mirror `ObjectOutputStream.scala` in `pylib-py/src/java/io/ObjectInputStream.scala`.
Use the same wire format the writer produces (currently `pickle` /
length-prefixed binary — confirm by reading the writer). Surface
`readInt`, `readObject`, `readBoolean`, `readUTF`, `close`. The fixtures
only exercise primitive/enum reads, so a minimal subset is enough.

After landing, rebuild support jars and verify the five fixtures flip.
