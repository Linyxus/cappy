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

class CharArrayWriter(initialSize: Int) extends Writer {
  if (initialSize < 0)
    throw new IllegalArgumentException("size must be >= 0")

  protected var buf: Array[Char] = new Array[Char](initialSize)
  protected var count: Int = 0

  def this() = this(32)

  override def close(): Unit = ()

  private def ensureCapacity(i: Int): Unit = {
    if (i > this.buf.length - this.count) {
      val newLen = Math.max(2 * this.buf.length, this.count + i)
      this.buf = java.util.Arrays.copyOf(this.buf, newLen)
    }
  }

  override def flush(): Unit = ()

  def reset(): Unit =
    this.count = 0

  def size(): Int =
    this.count

  def toCharArray(): Array[Char] =
    java.util.Arrays.copyOf(buf, count)

  override def toString(): String =
    new String(this.buf, 0, this.count)

  override def write(c: Array[Char], offset: Int, count: Int): Unit = {
    BoundsChecks.checkOffsetCount(offset, count, c.length)

    ensureCapacity(count)
    System.arraycopy(c, offset, this.buf, this.count, count)
    this.count += count
  }

  override def write(oneChar: Int): Unit = {
    ensureCapacity(1)
    this.buf(this.count) = oneChar.toChar
    this.count += 1
  }

  override def write(str: String, offset: Int, count: Int): Unit = {
    val endOffset = BoundsChecks.checkOffsetCount(offset, count, str.length())

    ensureCapacity(count)
    str.getChars(offset, endOffset, this.buf, this.count)
    this.count += count
  }

  def writeTo(out: Writer): Unit =
    out.write(this.buf, 0, count)

  override def append(c: Char): CharArrayWriter = {
    write(c)
    this
  }

  override def append(csq: CharSequence): CharArrayWriter = {
    if (csq == null) write("null")
    else write(csq.toString())
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): CharArrayWriter = {
    if (csq == null) write("null", start, end)
    else write(csq.subSequence(start, end).toString())
    this
  }
}
