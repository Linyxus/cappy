package java.io

class BufferedOutputStream(out: OutputStream, size: Int) extends FilterOutputStream(out) {
  if (size <= 0)
    throw new IllegalArgumentException("Buffer size <= 0")

  private var buf = new Array[Byte](size)
  private var count = 0

  def this(out: OutputStream) =
    this(out, 8192)

  override def write(b: Int): Unit = {
    if (count >= buf.length)
      flushBuffer()
    buf(count) = b.toByte
    count += 1
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    BoundsChecks.checkOffsetCount(off, len, b.length)
    if (len >= buf.length) {
      flushBuffer()
      out.write(b, off, len)
    } else {
      if (len > buf.length - count)
        flushBuffer()
      System.arraycopy(b, off, buf, count, len)
      count += len
    }
  }

  override def flush(): Unit = {
    flushBuffer()
    out.flush()
  }

  private def flushBuffer(): Unit =
    if (count > 0) {
      out.write(buf, 0, count)
      count = 0
    }
}
