import java.io.{InputStream, InputStreamReader, ByteArrayInputStream}
import java.lang.StringBuilder

/** Delivers `data` one byte at a time — forces the decoder to buffer
 *  partial multi-byte UTF-8 sequences across reads. */
final class ByteAtATimeStream(data: Array[Byte]) extends InputStream:
  private var pos = 0

  override def read(): Int =
    if pos >= data.length then -1
    else
      val b = data(pos) & 0xff
      pos += 1
      b

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    if len == 0 then 0
    else if pos >= data.length then -1
    else
      b(off) = data(pos)
      pos += 1
      1

  override def available(): Int =
    data.length - pos

@main def javalibIoInputStreamReader(): Unit =
  // Emoji U+1F600 = F0 9F 98 80 in UTF-8. Build the reference string
  // via `Character.toString(codePoint)` so it goes through
  // `PyBuiltins.chr_of` and becomes a real single-codepoint Python
  // string — a literal `"\uD83D\uDE00"` would emit as two separate
  // surrogate codepoints on ScalaPy and wouldn't compare equal to the
  // decoder's output. See `notes/javalib-string-literal-surrogate-emission.md`.
  val emoji = Character.toString(0x1F600)
  val emojiBytes = Array[Byte](0xF0.toByte, 0x9F.toByte, 0x98.toByte, 0x80.toByte)

  // Read one codepoint at a time through `read(): Int` — exercises
  // the incremental-decode path where each underlying 1-byte read
  // produces zero or one codepoint. Accumulate via
  // `appendCodePoint` so supplementary codepoints above U+FFFF
  // survive (plain `.toChar` truncates at 16 bits).
  val isr = new InputStreamReader(new ByteAtATimeStream(emojiBytes), "UTF-8")
  val collected = new StringBuilder()
  var next = isr.read()
  while next != -1 do
    collected.appendCodePoint(next)
    next = isr.read()
  isr.close()
  val decoded = collected.toString()
  println("partial-utf8:" + decoded.length() + ":" + (decoded == emoji))

  // Same payload via read(cbuf, off, len): buffer length larger than
  // total chars, so the decoder fills it gradually through repeated
  // 1-byte reads from the underlying stream.
  val isrArr = new InputStreamReader(new ByteAtATimeStream(emojiBytes), "UTF-8")
  val cbuf = new Array[Char](8)
  var total = 0
  var chunk = isrArr.read(cbuf, total, cbuf.length - total)
  while chunk != -1 do
    total += chunk
    chunk = isrArr.read(cbuf, total, cbuf.length - total)
  isrArr.close()
  println("partial-utf8-array:" + total + ":" + new String(cbuf, 0, total))

  // Mixed ASCII + multi-byte BMP codepoints — é (2 bytes) and 中
  // (3 bytes). All BMP, so no surrogate-literal drift; Scala and
  // Python see the same codepoint layout.
  val mixed = "a\u00e9b\u4e2dc"
  val mixedBytes = mixed.getBytes("UTF-8")
  val isrMixed = new InputStreamReader(new ByteAtATimeStream(mixedBytes), "UTF-8")
  val mixedBuf = new StringBuilder()
  var m = isrMixed.read()
  while m != -1 do
    mixedBuf.appendCodePoint(m)
    m = isrMixed.read()
  println("mixed:" + (mixedBuf.toString() == mixed))

  // getEncoding() returns the Java canonical name while open, null when closed.
  val enc = new InputStreamReader(new ByteArrayInputStream(Array[Byte](65)), "UTF-8")
  val encOpen = enc.getEncoding()
  enc.close()
  val encClosed = enc.getEncoding()
  println("encoding:" + encOpen + ":" + (encClosed == null))

  // mark() on InputStreamReader is not supported by our streaming port.
  val markIsr = new InputStreamReader(new ByteArrayInputStream("xyz".getBytes("UTF-8")), "UTF-8")
  val markSup = markIsr.markSupported()
  val markThrew =
    try
      markIsr.mark(4)
      false
    catch
      case _: java.io.IOException => true
  println("mark-not-supported:" + markSup + ":" + markThrew)

  // Closed reader rejects further reads.
  val closedIsr = new InputStreamReader(new ByteArrayInputStream(Array[Byte](65, 66)), "UTF-8")
  closedIsr.close()
  val closedThrew =
    try
      closedIsr.read()
      false
    catch
      case _: java.io.IOException => true
  println("closed:" + closedThrew)
