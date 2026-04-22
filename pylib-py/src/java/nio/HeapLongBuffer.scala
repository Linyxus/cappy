package java.nio

private[nio] final class HeapLongBuffer private (
    _capacity: Int,
    _array0: Array[Long],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends LongBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): LongBuffer = {
    val newCapacity = remaining()
    new HeapLongBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): LongBuffer = {
    val result = new HeapLongBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): LongBuffer = {
    val result = new HeapLongBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }
  @noinline def get(): Long = GenBuffer(this).getElem()
  @noinline def put(l: Long): LongBuffer = GenBuffer(this).putElem(l)
  @noinline def get(index: Int): Long = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, l: Long): LongBuffer = GenBuffer(this).putAt(index, l)
  @noinline override def get(dst: Array[Long], offset: Int, length: Int): LongBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Long], offset: Int, length: Int): LongBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): LongBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Long = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Long): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Long], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Long], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapLongBuffer {
  private[nio] implicit object NewHeapLongBuffer extends GenHeapBuffer.NewHeapBuffer[LongBuffer, Long] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Long],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): LongBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapLongBuffer")
      new HeapLongBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(
      array: Array[Long],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): LongBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
