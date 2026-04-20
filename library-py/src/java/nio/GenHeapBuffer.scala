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

import java.lang.{reflect => jlr}

private[nio] object GenHeapBuffer {
  def apply[B <: Buffer](buffer: B): GenHeapBuffer[B] =
    new GenHeapBuffer(buffer)

  trait NewHeapBuffer[BufferType <: Buffer, ElementType] {
    def apply(
        capacity: Int,
        array: Array[ElementType],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): BufferType
  }

  @inline
  def generic_wrap[BufferType <: Buffer, ElementType](
      array: Array[ElementType],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  )(implicit newHeapBuffer: NewHeapBuffer[BufferType, ElementType]): BufferType = {
    BoundsChecks.checkOffsetCount(arrayOffset, capacity, jlr.Array.getLength(array.asInstanceOf[AnyRef]))
    val initialLimit = BoundsChecks.checkOffsetCount(initialPosition, initialLength, capacity)
    newHeapBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, isReadOnly, false)
  }
}

private[nio] final class GenHeapBuffer[B <: Buffer] private (val owner: B) extends AnyVal {
  import owner._

  type NewThisHeapBuffer = GenHeapBuffer.NewHeapBuffer[BufferType, ElementType]

  @inline
  def sliceBuffer()(implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val newCapacity = remaining()
    newHeapBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly(), isDirect())
  }

  @inline
  def duplicateBuffer()(implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val result = newHeapBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly(), isDirect())
    result._mark = _mark
    result
  }

  @inline
  def readOnlyBuffer()(implicit newHeapBuffer: NewThisHeapBuffer): BufferType = {
    val result = newHeapBuffer(capacity(), _array, _arrayOffset, position(), limit(), true, isDirect())
    result._mark = _mark
    result
  }

  @inline
  def compactBuffer(): BufferType = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    owner
  }

  @inline
  def loadAt(index: Int): ElementType =
    jlr.Array.get(_array.asInstanceOf[AnyRef], _arrayOffset + index).asInstanceOf[ElementType]

  @inline
  def storeAt(index: Int, elem: ElementType): Unit =
    jlr.Array.set(_array.asInstanceOf[AnyRef], _arrayOffset + index, elem.asInstanceOf[AnyRef])

  @inline
  def loadInto(startIndex: Int, dst: Array[ElementType], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)

  @inline
  def storeFrom(startIndex: Int, src: Array[ElementType], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}
