# `java.io.PushbackReader` is missing from pylib

## Minimal example for reproducing

```scala
import java.io.{PushbackReader, StringReader}

@main def Test =
  val r = new PushbackReader(new StringReader("abc"))
  val c = r.read()
  r.unread(c)
  println(r.read().toChar)  // expect: a
```

Surfaced in PyRunTests run3 logs.

## Output vs Expected Behaviour

```
Unresolved class 'java.io.PushbackReader'
Compilation failed for: '<fixture>.scala'
```

Expected: prints `a`.

## Quick Analysis

`PushbackReader` is a `FilterReader` with a small pushback buffer. ~15-20
lines to port.

**Fix shape**: add `pylib-py/src/java/io/PushbackReader.scala`:

```scala
class PushbackReader(in: Reader, sz: Int) extends Reader:
  def this(in: Reader) = this(in, 1)
  private val buf: Array[Char] = new Array[Char](sz)
  private var pos: Int = sz  // empty when pos == sz

  override def read(): Int =
    if pos < sz then { val c = buf(pos); pos += 1; c.toInt }
    else in.read()

  def unread(c: Int): Unit =
    if pos == 0 then throw new java.io.IOException("Pushback buffer overflow")
    else { pos -= 1; buf(pos) = c.toChar }

  // also: read(cbuf, off, len), unread(cbuf), unread(cbuf, off, len),
  // ready(), close() — see JDK javadoc
```

Specialist report: `/tmp/pyrun-analysis/cat-b-other-linker.md` (port sketch).
