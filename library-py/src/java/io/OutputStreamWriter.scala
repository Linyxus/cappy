package java.io

import java.lang.StringBuilder
import java.nio.charset.{Charset, UnsupportedCharsetException}

import scala.python.runtime.PyBuiltins

class OutputStreamWriter(private var out: OutputStream | Null, charsetName: String | Null)
    extends Writer {

  import OutputStreamWriter._

  private val encoding = normalizeEncoding(charsetName)
  private var closed = false
  private var pendingHighSurrogate: Char = 0.toChar
  private var hasPendingHighSurrogate = false

  def this(out: OutputStream) =
    this(out, null)

  def getEncoding(): String | Null =
    if (closed) null else encoding

  override def write(c: Int): Unit =
    write(PyBuiltins.chr_of(c), 0, 1)

  override def write(cbuf: Array[Char], off: Int, count: Int): Unit =
    write(new String(cbuf, off, count), 0, count)

  override def write(str: String, off: Int, count: Int): Unit = {
    ensureOpen()
    BoundsChecks.checkOffsetCount(off, count, str.length())
    if (count == 0) return
    writeChunk(str.substring(off, off + count), endOfInput = false)
  }

  override def flush(): Unit = {
    ensureOpen()
    out.asInstanceOf[OutputStream].flush()
  }

  override def close(): Unit = {
    if (!closed) {
      if (hasPendingHighSurrogate)
        emit("\uFFFD")
      out.asInstanceOf[OutputStream].flush()
      out.asInstanceOf[OutputStream].close()
      closed = true
      out = null
      hasPendingHighSurrogate = false
    }
  }

  private def writeChunk(chunk: String, endOfInput: Boolean): Unit = {
    val prefix =
      if (hasPendingHighSurrogate) String.valueOf(pendingHighSurrogate)
      else ""
    hasPendingHighSurrogate = false

    val text = prefix + chunk
    if (text.isEmpty)
      ()
    else {
      val builder = new StringBuilder()
      var i = 0
      while (i < text.length()) {
        val ch = text.charAt(i)
        if (Character.isHighSurrogate(ch)) {
          if (i + 1 < text.length() && Character.isLowSurrogate(text.charAt(i + 1))) {
            builder.append(PyBuiltins.chr_of(Character.toCodePoint(ch, text.charAt(i + 1))))
            i += 2
          } else if (i + 1 >= text.length() && !endOfInput) {
            pendingHighSurrogate = ch
            hasPendingHighSurrogate = true
            i = text.length()
          } else {
            builder.append('\uFFFD')
            i += 1
          }
        } else if (Character.isLowSurrogate(ch)) {
          builder.append('\uFFFD')
          i += 1
        } else {
          builder.append(ch)
          i += 1
        }
      }

      val rendered = builder.toString()
      if (rendered.length() > 0)
        emit(rendered)
    }
  }

  private def emit(text: String): Unit =
    out.asInstanceOf[OutputStream].write(PyBuiltins.encode_bytes(text, encoding))

  private def ensureOpen(): Unit =
    if (closed)
      throw new IOException("Closed writer.")
}

object OutputStreamWriter {
  private[io] def normalizeEncoding(charsetName: String | Null): String =
    val effective = if charsetName == null then "UTF-8" else charsetName
    try Charset.forName(effective).name()
    catch
      case _: UnsupportedCharsetException =>
        throw new UnsupportedEncodingException(effective)
}
