// Mirrors `tests/run/t8690.scala`: the upstream regression that
// `scala.io.Source.fromInputStream` returns the right characters
// after a `toString` peek. The Python backend additionally needs
// the `(InputStream, Charset)` and `(InputStream, CharsetDecoder)`
// constructors on `java.io.InputStreamReader` that
// `BufferedSource.reader()` calls.

import scala.io.Source
import java.io.{ByteArrayInputStream, InputStreamReader}
import java.nio.charset.{Charset, StandardCharsets}

@main def inputstreamReaderCharsetCtors(): Unit =
  val txt = "abcdef"

  // (InputStream, CharsetDecoder) ctor — the path BufferedSource.reader()
  // takes via Codec#decoder.
  val in1 = new ByteArrayInputStream(txt.getBytes())
  val src = Source.fromInputStream(in1)
  println(src.toString)        // forces the BufferedSource to peek the head
  println(src.mkString)        // must still yield the full input

  // (InputStream, Charset) ctor — direct.
  val in2 = new ByteArrayInputStream(txt.getBytes())
  val r2 = new InputStreamReader(in2, StandardCharsets.UTF_8)
  println(r2.getEncoding())    // UTF-8

  // (InputStream, CharsetDecoder) ctor — direct.
  val in3 = new ByteArrayInputStream(txt.getBytes())
  val r3 = new InputStreamReader(in3, Charset.forName("UTF-8").newDecoder())
  println(r3.getEncoding())    // UTF-8
