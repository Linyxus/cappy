/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.io

class BufferedReader(in: Reader, sz: Int) extends Reader {
  def this(in: Reader) = this(in, 4096)

  private[this] var buf: Array[Char] = new Array[Char](sz)
  private[this] var end = 0
  private[this] var pos = 0
  private[this] var closed = false
  private[this] var validMark = false

  override def close(): Unit = {
    if (!closed) {
      closed = true
      in.close()
    }
  }

  override def mark(readAheadLimit: Int): Unit = {
    if (readAheadLimit < 0)
      throw new IllegalArgumentException("Read-ahead limit < 0")
    ensureOpen()

    val srcBuf = buf
    if (buf.length < readAheadLimit)
      buf = new Array[Char](readAheadLimit)

    if (pos != 0 || (buf ne srcBuf))
      System.arraycopy(srcBuf, pos, buf, 0, end - pos)

    end -= pos
    pos = 0
    validMark = true
  }

  override def markSupported(): Boolean =
    true

  override def read(): Int = {
    ensureOpen()
    if (prepareRead()) {
      val res = buf(pos).toInt
      pos += 1
      res
    } else -1
  }

  override def read(cbuf: Array[Char], off: Int, count: Int): Int = {
    ensureOpen()
    BoundsChecks.checkOffsetCount(off, count, cbuf.length)

    if (count == 0) 0
    else if (prepareRead()) {
      val charsRead = Math.min(count, end - pos)
      System.arraycopy(this.buf, pos, cbuf, off, charsRead)
      pos += charsRead
      charsRead
    } else -1
  }

  def readLine(): String | Null = {
    ensureOpen()
    var res = ""

    while (prepareRead() && buf(pos) != '\n' && buf(pos) != '\r') {
      res += buf(pos)
      pos += 1
    }

    if (pos >= end) {
      if (res == "") null
      else res
    } else {
      pos += 1
      if (buf(pos - 1) == '\r' && prepareRead() && buf(pos) == '\n')
        pos += 1
      res
    }
  }

  override def ready(): Boolean = {
    ensureOpen()
    pos < end || in.ready()
  }

  override def reset(): Unit = {
    ensureOpen()
    if (!validMark) throw new IOException("Mark invalid")
    pos = 0
  }

  override def skip(n: Long): Long = {
    if (n < 0) {
      throw new IllegalArgumentException("n negative")
    } else {
      ensureOpen()
      if (pos < end) {
        val count = Math.min(n, end - pos).toInt
        pos += count
        count.toLong
      } else {
        validMark = false
        in.skip(n)
      }
    }
  }

  private def prepareRead(): Boolean =
    pos < end || fillBuffer()

  private def fillBuffer(): Boolean = {
    if (validMark && end < buf.length) {
      val read = in.read(buf, end, buf.length - end)
      if (read > 0)
        end += read
      read > 0
    } else {
      validMark = false
      end = in.read(buf)
      pos = 0
      end > 0
    }
  }

  private def ensureOpen(): Unit =
    if (closed)
      throw new IOException("Operation on closed stream")
}
