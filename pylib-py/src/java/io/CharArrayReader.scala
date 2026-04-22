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

class CharArrayReader(protected var buf: Array[Char] | Null, offset: Int, length: Int) extends Reader {
  if ((offset | length | (offset + length) | (buf.asInstanceOf[Array[Char]].length - offset)) < 0)
    throw new IllegalArgumentException

  protected var pos: Int = offset
  protected var markedPos: Int = offset
  protected var count: Int = Math.min(offset + length, buf.asInstanceOf[Array[Char]].length)

  def this(buf: Array[Char]) = this(buf, 0, buf.length)

  override def close(): Unit =
    this.buf = null

  override def mark(readAheadLimit: Int): Unit = {
    ensureOpen()
    this.markedPos = this.pos
  }

  override def markSupported(): Boolean =
    true

  override def read(): Int = {
    ensureOpen()
    val buf0 = buf.asInstanceOf[Array[Char]]

    if (this.pos == this.count) {
      -1
    } else {
      this.pos += 1
      buf0(this.pos - 1)
    }
  }

  override def read(buffer: Array[Char], offset: Int, len: Int): Int = {
    BoundsChecks.checkOffsetCount(offset, len, buffer.length)
    ensureOpen()
    val buf0 = buf.asInstanceOf[Array[Char]]

    if (len == 0) {
      0
    } else if (this.pos < this.count) {
      val bytesRead = Math.min(len, this.count - this.pos)
      System.arraycopy(buf0, this.pos, buffer, offset, bytesRead)
      this.pos += bytesRead
      bytesRead
    } else {
      -1
    }
  }

  override def ready(): Boolean = {
    ensureOpen()
    this.pos != this.count
  }

  override def reset(): Unit = {
    ensureOpen()
    this.pos = this.markedPos
  }

  override def skip(n: Long): Long = {
    ensureOpen()

    val available: Long = (this.count - this.pos).toLong
    val skipped: Long = Math.max(0L, Math.min(n, available))
    this.pos += skipped.toInt
    skipped
  }

  private def ensureOpen(): Unit =
    if (this.buf == null)
      throw new IOException("CharArrayReader is closed.")
}
