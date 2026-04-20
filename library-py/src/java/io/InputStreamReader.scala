package java.io

import java.util.Arrays

import scala.python.runtime.{PyBytes, PyCodecs}

class InputStreamReader(private var in: InputStream | Null, charsetName: String | Null)
    extends Reader {

  private val encoding = OutputStreamWriter.normalizeEncoding(charsetName)
  private val decoder = PyCodecs.newUtf8Decoder()
  private val readBuffer = new Array[Byte](4096)

  private var closed = false
  private var pending: String = ""
  private var pendingPos: Int = 0
  private var inputExhausted: Boolean = false
  private var decoderFlushed: Boolean = false

  def this(in: InputStream) =
    this(in, null)

  def getEncoding(): String | Null =
    // The encoding passed to Python codecs is lowercase ("utf-8"), but
    // Java's `getEncoding()` contract returns the canonical charset name
    // — `"UTF-8"` is the Java canonical form, matching what the JDK's
    // `StandardCharsets.UTF_8.name()` would return.
    if (closed) null
    else if (encoding == "utf-8") "UTF-8"
    else encoding

  override def read(): Int = {
    ensureOpen()
    if (!ensureChars()) -1
    else {
      val c = pending.charAt(pendingPos).toInt
      pendingPos += 1
      c
    }
  }

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    ensureOpen()
    BoundsChecks.checkOffsetCount(off, len, cbuf.length)
    if (len == 0) 0
    else if (!ensureChars()) -1
    else {
      val available = pending.length() - pendingPos
      val toCopy = Math.min(available, len)
      var i = 0
      while (i < toCopy) {
        cbuf(off + i) = pending.charAt(pendingPos + i)
        i += 1
      }
      pendingPos += toCopy
      toCopy
    }
  }

  override def ready(): Boolean = {
    ensureOpen()
    pendingPos < pending.length() || (!inputExhausted && in.asInstanceOf[InputStream].available() > 0)
  }

  override def markSupported(): Boolean =
    false

  override def close(): Unit = {
    if (!closed) {
      closed = true
      if (in != null)
        in.asInstanceOf[InputStream].close()
      in = null
      pending = ""
      pendingPos = 0
    }
  }

  private def ensureChars(): Boolean = {
    if (pendingPos < pending.length()) return true
    pending = ""
    pendingPos = 0
    while (pending.length() == 0 && !decoderFlushed)
      if (inputExhausted)
        pending = decoder.decode(PyBytes.toPyBytes(new Array[Byte](0)), true)
        decoderFlushed = true
      else
        val n = in.asInstanceOf[InputStream].read(readBuffer)
        if (n < 0)
          inputExhausted = true
        else if (n > 0)
          val chunk =
            if (n == readBuffer.length) readBuffer
            else Arrays.copyOf(readBuffer, n)
          pending = decoder.decode(PyBytes.toPyBytes(chunk), false)
    pending.length() > 0
  }

  private def ensureOpen(): Unit =
    if (closed)
      throw new IOException("Stream closed")
}
