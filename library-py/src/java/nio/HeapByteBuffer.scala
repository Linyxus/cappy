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

private[nio] final class HeapByteBuffer private (
    _capacity: Int,
    _array0: Array[Byte],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean,
    _isDirect: Boolean
) extends ByteBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = _isDirect

  @noinline
  def slice(): ByteBuffer = {
    val newCapacity = remaining()
    new HeapByteBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly(), isDirect())
  }

  @noinline
  def duplicate(): ByteBuffer = {
    val result = new HeapByteBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly(), isDirect())
    result._mark = _mark
    result
  }

  @noinline
  def asReadOnlyBuffer(): ByteBuffer = {
    val result = new HeapByteBuffer(capacity(), _array, _arrayOffset, position(), limit(), true, isDirect())
    result._mark = _mark
    result
  }

  @noinline
  def get(): Byte =
    GenBuffer(this).getElem()

  @noinline
  def put(b: Byte): ByteBuffer =
    GenBuffer(this).putElem(b)

  @noinline
  def get(index: Int): Byte =
    GenBuffer(this).getAt(index)

  @noinline
  def put(index: Int, b: Byte): ByteBuffer =
    GenBuffer(this).putAt(index, b)

  @noinline
  override def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).getArray(dst, offset, length)

  @noinline
  override def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).putArray(src, offset, length)

  @noinline
  def compact(): ByteBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  @inline
  private def arrayBits: ByteArrayBits =
    ByteArrayBits(_array, _arrayOffset, isBigEndian)

  @noinline def getChar(): Char =
    arrayBits.loadChar(getPosAndAdvanceRead(2))

  @noinline def putChar(value: Char): ByteBuffer =
    multiByteRelWrite(bytes = 2)(arrayBits.storeChar(_, value))

  @noinline def getChar(index: Int): Char =
    arrayBits.loadChar(validateIndex(index, 2))

  @noinline def putChar(index: Int, value: Char): ByteBuffer =
    multiByteAbsWrite(bytes = 2, index)(arrayBits.storeChar(_, value))

  def asCharBuffer(): CharBuffer =
    HeapByteBufferCharView.fromHeapByteBuffer(this)

  @noinline def getShort(): Short =
    arrayBits.loadShort(getPosAndAdvanceRead(2))

  @noinline def putShort(value: Short): ByteBuffer =
    multiByteRelWrite(bytes = 2)(arrayBits.storeShort(_, value))

  @noinline def getShort(index: Int): Short =
    arrayBits.loadShort(validateIndex(index, 2))

  @noinline def putShort(index: Int, value: Short): ByteBuffer =
    multiByteAbsWrite(bytes = 2, index)(arrayBits.storeShort(_, value))

  def asShortBuffer(): ShortBuffer =
    HeapByteBufferShortView.fromHeapByteBuffer(this)

  @noinline def getInt(): Int =
    arrayBits.loadInt(getPosAndAdvanceRead(4))

  @noinline def putInt(value: Int): ByteBuffer =
    multiByteRelWrite(bytes = 4)(arrayBits.storeInt(_, value))

  @noinline def getInt(index: Int): Int =
    arrayBits.loadInt(validateIndex(index, 4))

  @noinline def putInt(index: Int, value: Int): ByteBuffer =
    multiByteAbsWrite(bytes = 4, index)(arrayBits.storeInt(_, value))

  def asIntBuffer(): IntBuffer =
    HeapByteBufferIntView.fromHeapByteBuffer(this)

  @noinline def getLong(): Long =
    arrayBits.loadLong(getPosAndAdvanceRead(8))

  @noinline def putLong(value: Long): ByteBuffer =
    multiByteRelWrite(bytes = 8)(arrayBits.storeLong(_, value))

  @noinline def getLong(index: Int): Long =
    arrayBits.loadLong(validateIndex(index, 8))

  @noinline def putLong(index: Int, value: Long): ByteBuffer =
    multiByteAbsWrite(bytes = 8, index)(arrayBits.storeLong(_, value))

  def asLongBuffer(): LongBuffer =
    HeapByteBufferLongView.fromHeapByteBuffer(this)

  @noinline def getFloat(): Float =
    arrayBits.loadFloat(getPosAndAdvanceRead(4))

  @noinline def putFloat(value: Float): ByteBuffer =
    multiByteRelWrite(bytes = 4)(arrayBits.storeFloat(_, value))

  @noinline def getFloat(index: Int): Float =
    arrayBits.loadFloat(validateIndex(index, 4))

  @noinline def putFloat(index: Int, value: Float): ByteBuffer =
    multiByteAbsWrite(bytes = 4, index)(arrayBits.storeFloat(_, value))

  def asFloatBuffer(): FloatBuffer =
    HeapByteBufferFloatView.fromHeapByteBuffer(this)

  @noinline def getDouble(): Double =
    arrayBits.loadDouble(getPosAndAdvanceRead(8))

  @noinline def putDouble(value: Double): ByteBuffer =
    multiByteRelWrite(bytes = 8)(arrayBits.storeDouble(_, value))

  @noinline def getDouble(index: Int): Double =
    arrayBits.loadDouble(validateIndex(index, 8))

  @noinline def putDouble(index: Int, value: Double): ByteBuffer =
    multiByteAbsWrite(bytes = 8, index)(arrayBits.storeDouble(_, value))

  def asDoubleBuffer(): DoubleBuffer =
    HeapByteBufferDoubleView.fromHeapByteBuffer(this)

  @inline
  private[nio] def load(index: Int): Byte =
    _array(_arrayOffset + index)

  @inline
  private[nio] def store(index: Int, elem: Byte): Unit =
    _array(_arrayOffset + index) = elem

  @inline
  override private[nio] def load(startIndex: Int, dst: Array[Byte], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int, src: Array[Byte], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapByteBuffer {
  private[nio] implicit object NewHeapByteBuffer extends GenHeapBuffer.NewHeapBuffer[ByteBuffer, Byte] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Byte],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): ByteBuffer =
      new HeapByteBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly, direct)
  }

  @noinline
  private[nio] def wrap(
      array: Array[Byte],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): ByteBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)

  private[nio] def allocateDirect(capacity: Int): ByteBuffer =
    new HeapByteBuffer(capacity, new Array[Byte](capacity), 0, 0, capacity, false, true)
}
