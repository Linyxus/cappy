package java.io

class PushbackReader(in: Reader, sz: Int) extends FilterReader(in) {
  def this(in: Reader) = this(in, 1)

  if (sz <= 0)
    throw new IllegalArgumentException("size <= 0")

  private[this] val buf: Array[Char] = new Array[Char](sz)
  private[this] var pos: Int = sz

  override def read(): Int = {
    if (pos < sz) {
      val c = buf(pos).toInt
      pos += 1
      c
    } else {
      in.read()
    }
  }

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    BoundsChecks.checkOffsetCount(off, len, cbuf.length)
    if (len == 0) 0
    else {
      var written = 0
      val available = sz - pos
      if (available > 0) {
        val n = Math.min(available, len)
        System.arraycopy(buf, pos, cbuf, off, n)
        pos += n
        written = n
      }
      if (written < len) {
        val read = in.read(cbuf, off + written, len - written)
        if (read > 0) written += read
        else if (written == 0) return read
      }
      written
    }
  }

  def unread(c: Int): Unit = {
    if (pos == 0)
      throw new IOException("Pushback buffer overflow")
    pos -= 1
    buf(pos) = c.toChar
  }

  def unread(cbuf: Array[Char], off: Int, len: Int): Unit = {
    BoundsChecks.checkOffsetCount(off, len, cbuf.length)
    if (len > pos)
      throw new IOException("Pushback buffer overflow")
    pos -= len
    System.arraycopy(cbuf, off, buf, pos, len)
  }

  def unread(cbuf: Array[Char]): Unit =
    unread(cbuf, 0, cbuf.length)

  override def ready(): Boolean =
    pos < sz || in.ready()

  override def markSupported(): Boolean = false

  override def mark(readAheadLimit: Int): Unit =
    throw new IOException("mark/reset not supported")

  override def reset(): Unit =
    throw new IOException("mark/reset not supported")

  override def skip(n: Long): Long = {
    if (n < 0)
      throw new IllegalArgumentException("skip value is negative")
    val available = sz - pos
    if (available > 0) {
      val skipped = Math.min(available.toLong, n).toInt
      pos += skipped
      if (skipped.toLong == n) n
      else skipped.toLong + in.skip(n - skipped.toLong)
    } else {
      in.skip(n)
    }
  }

  override def close(): Unit =
    in.close()
}
