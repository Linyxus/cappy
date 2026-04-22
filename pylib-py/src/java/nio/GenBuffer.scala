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

package java.nio

import scala.language.unsafeNulls

import java.lang.{reflect => jlr}
import java.util.function._

private[nio] object GenBuffer {
  def apply[B <: Buffer](buffer: B): GenBuffer[B] =
    new GenBuffer(buffer)
}

private[nio] final class GenBuffer[B <: Buffer] private (val owner: B) extends AnyVal {
  import owner._

  @inline
  def getElem(): ElementType =
    load(getPosAndAdvanceRead())

  @inline
  def putElem(elem: ElementType): B = {
    ensureNotReadOnly()
    store(getPosAndAdvanceWrite(), elem)
    owner
  }

  @inline
  def getAt(index: Int): ElementType = {
    BoundsChecks.checkIndex(index, limit())
    load(index)
  }

  @inline
  def putAt(index: Int, elem: ElementType): BufferType = {
    ensureNotReadOnly()
    BoundsChecks.checkIndex(index, limit())
    store(index, elem)
    owner
  }

  @inline
  def getArray(dst: Array[ElementType], offset: Int, length: Int): BufferType = {
    BoundsChecks.checkOffsetCount(offset, length, jlr.Array.getLength(dst.asInstanceOf[AnyRef]))
    load(getPosAndAdvanceRead(length), dst, offset, length)
    owner
  }

  @inline
  def putBuffer(src: BufferType): BufferType = {
    if (src eq owner)
      throw new IllegalArgumentException
    ensureNotReadOnly()
    val srcLimit = src.limit()
    var srcPos = src.position()
    val length = srcLimit - srcPos
    var selfPos = getPosAndAdvanceWrite(length)
    src.position(srcLimit)

    val srcArray = src._array
    if (srcArray != null) {
      store(selfPos, srcArray, src._arrayOffset + srcPos, length)
    } else {
      while (srcPos != srcLimit) {
        store(selfPos, src.load(srcPos))
        srcPos += 1
        selfPos += 1
      }
    }

    owner
  }

  @inline
  def putArray(src: Array[ElementType], offset: Int, length: Int): BufferType = {
    ensureNotReadOnly()
    BoundsChecks.checkOffsetCount(offset, length, jlr.Array.getLength(src.asInstanceOf[AnyRef]))
    store(getPosAndAdvanceWrite(length), src, offset, length)
    owner
  }

  @inline
  def generic_hasArray(): Boolean =
    _array != null && !isReadOnly()

  @inline
  def generic_array(): Array[ElementType] = {
    val a = _array
    if (a == null)
      throw new UnsupportedOperationException
    if (isReadOnly())
      throw new ReadOnlyBufferException
    a
  }

  @inline
  def generic_arrayOffset(): Int = {
    val o = _arrayOffset
    if (o == -1)
      throw new UnsupportedOperationException
    if (isReadOnly())
      throw new ReadOnlyBufferException
    o
  }

  @inline
  def generic_hashCode(hashSeed: Int): Int = {
    import java.util.internal.MurmurHash3._
    val start = position()
    val end = limit()
    var h = hashSeed
    var i = start
    while (i != end) {
      h = mix(h, load(i).hashCode())
      i += 1
    }
    finalizeHash(h, end - start)
  }

  @inline
  def generic_compareTo(that: BufferType)(compare: BiFunction[ElementType, ElementType, Int]): Int = {
    if (owner eq that) {
      0
    } else {
      val thisStart = owner.position()
      val thisRemaining = owner.limit() - thisStart
      val thatStart = that.position()
      val thatRemaining = that.limit() - thatStart
      val shortestLength = Math.min(thisRemaining, thatRemaining)

      var i = 0
      while (i != shortestLength) {
        val cmp = compare(owner.load(thisStart + i), that.load(thatStart + i))
        if (cmp != 0)
          return cmp
        i += 1
      }

      Integer.compare(thisRemaining, thatRemaining)
    }
  }

  @inline
  def loadInto(startIndex: Int, dst: Array[ElementType], offset: Int, length: Int): Unit = {
    var selfPos = startIndex
    val endPos = selfPos + length
    var arrayIndex = offset
    val dstRef = dst.asInstanceOf[AnyRef]
    while (selfPos != endPos) {
      jlr.Array.set(dstRef, arrayIndex, load(selfPos).asInstanceOf[AnyRef])
      selfPos += 1
      arrayIndex += 1
    }
  }

  @inline
  def storeFrom(startIndex: Int, src: Array[ElementType], offset: Int, length: Int): Unit = {
    var selfPos = startIndex
    val endPos = selfPos + length
    var arrayIndex = offset
    val srcRef = src.asInstanceOf[AnyRef]
    while (selfPos != endPos) {
      store(selfPos, jlr.Array.get(srcRef, arrayIndex).asInstanceOf[ElementType])
      selfPos += 1
      arrayIndex += 1
    }
  }
}
