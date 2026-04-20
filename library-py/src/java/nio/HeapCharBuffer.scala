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

private[nio] final class HeapCharBuffer private (
    _capacity: Int,
    _array0: Array[Char],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends CharBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): CharBuffer = {
    val newCapacity = remaining()
    new HeapCharBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): CharBuffer = {
    val result = new HeapCharBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): CharBuffer = {
    val result = new HeapCharBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }

  def subSequence(start: Int, end: Int): CharBuffer = {
    BoundsChecks.checkStartEnd(start, end, remaining())
    new HeapCharBuffer(capacity(), _array, _arrayOffset, position() + start, position() + end, isReadOnly())
  }

  @noinline def get(): Char = GenBuffer(this).getElem()
  @noinline def put(c: Char): CharBuffer = GenBuffer(this).putElem(c)
  @noinline def get(index: Int): Char = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, c: Char): CharBuffer = GenBuffer(this).putAt(index, c)
  @noinline override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Char], offset: Int, length: Int): CharBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): CharBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Char = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Char): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Char], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Char], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapCharBuffer {
  private[nio] implicit object NewHeapCharBuffer extends GenHeapBuffer.NewHeapBuffer[CharBuffer, Char] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Char],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): CharBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapCharBuffer")
      new HeapCharBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  private[nio] def wrap(
      array: Array[Char],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): CharBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
