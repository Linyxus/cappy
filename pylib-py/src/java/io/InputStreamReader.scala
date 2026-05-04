package java.io

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CharsetDecoder}
import java.util.Arrays

class InputStreamReader(private var in: InputStream | Null, charsetName: String | Null)
    extends Reader {

  private val encoding = OutputStreamWriter.normalizeEncoding(charsetName)
  private val charset = Charset.forName(encoding)
  private val readBuffer = new Array[Byte](4096)
  private var carryBytes = new Array[Byte](0)

  private var closed = false
  private var pending: String = ""
  private var pendingPos: Int = 0
  private var inputExhausted: Boolean = false
  private var decoderFlushed: Boolean = false

  def this(in: InputStream) =
    this(in, null)

  def this(in: InputStream, cs: Charset) =
    this(in, cs.name())

  def this(in: InputStream, dec: CharsetDecoder) =
    this(in, dec.charset().name())

  def getEncoding(): String | Null =
    if closed then null else encoding

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
    else {
      var copied = 0
      var done = false
      while !done && copied < len do
        if !ensureChars() then
          done = true
        else
          val available = pending.length() - pendingPos
          val toCopy = Math.min(available, len - copied)
          var i = 0
          while i < toCopy do
            cbuf(off + copied + i) = pending.charAt(pendingPos + i)
            i += 1
          pendingPos += toCopy
          copied += toCopy
          if pendingPos < pending.length() then
            done = true
      if copied == 0 then -1 else copied
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
      carryBytes = new Array[Byte](0)
    }
  }

  private def ensureChars(): Boolean = {
    if (pendingPos < pending.length()) return true
    pending = ""
    pendingPos = 0
    while (pending.length() == 0 && !decoderFlushed)
      if (inputExhausted)
        pending = decodeChunk(new Array[Byte](0), endOfInput = true)
        decoderFlushed = true
      else
        val n = in.asInstanceOf[InputStream].read(readBuffer)
        if (n < 0)
          inputExhausted = true
        else if (n > 0)
          val chunk =
            if (n == readBuffer.length) readBuffer
            else Arrays.copyOf(readBuffer, n)
          pending = decodeChunk(chunk, endOfInput = false)
    pending.length() > 0
  }

  private def decodeChunk(chunk: Array[Byte], endOfInput: Boolean): String = {
    val combined = combineCarry(chunk)
    if endOfInput then
      carryBytes = new Array[Byte](0)
      return charset.decode(ByteBuffer.wrap(combined)).toString()

    val decoder = charset.newDecoder()
    val inBuf = ByteBuffer.wrap(combined)
    var outBuf = CharBuffer.allocate(Math.max(1, combined.length * 2 + 2))

    def grow(current: CharBuffer): CharBuffer = {
      val next =
        if (current.capacity() == 0) CharBuffer.allocate(1)
        else CharBuffer.allocate(current.capacity() * 2)
      current.flip()
      next.put(current)
      next
    }

    var result = decoder.decode(inBuf, outBuf, endOfInput)
    while result.isOverflow() do
      outBuf = grow(outBuf)
      result = decoder.decode(inBuf, outBuf, endOfInput)
    if result.isError() then
      result.throwException()

    if endOfInput then
      var flushResult = decoder.flush(outBuf)
      while flushResult.isOverflow() do
        outBuf = grow(outBuf)
        flushResult = decoder.flush(outBuf)
      if flushResult.isError() then
        flushResult.throwException()

    carryBytes = extractRemaining(inBuf)
    outBuf.flip()
    outBuf.toString()
  }

  private def combineCarry(chunk: Array[Byte]): Array[Byte] =
    if carryBytes.length == 0 then
      chunk
    else if chunk.length == 0 then
      carryBytes
    else
      val combined = new Array[Byte](carryBytes.length + chunk.length)
      var i = 0
      while i < carryBytes.length do
        combined(i) = carryBytes(i)
        i += 1
      var j = 0
      while j < chunk.length do
        combined(carryBytes.length + j) = chunk(j)
        j += 1
      combined

  private def extractRemaining(inBuf: ByteBuffer): Array[Byte] = {
    val remaining = inBuf.remaining()
    val out = new Array[Byte](remaining)
    if remaining > 0 then
      inBuf.get(out)
    out
  }

  private def ensureOpen(): Unit =
    if (closed)
      throw new IOException("Stream closed")
}
