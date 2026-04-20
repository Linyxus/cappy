package java.nio

private[nio] final class HeapShortBuffer private (
    _capacity: Int,
    _array0: Array[Short],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends ShortBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): ShortBuffer = {
    val newCapacity = remaining()
    new HeapShortBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): ShortBuffer = {
    val result = new HeapShortBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): ShortBuffer = {
    val result = new HeapShortBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }
  @noinline def get(): Short = GenBuffer(this).getElem()
  @noinline def put(s: Short): ShortBuffer = GenBuffer(this).putElem(s)
  @noinline def get(index: Int): Short = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, s: Short): ShortBuffer = GenBuffer(this).putAt(index, s)
  @noinline override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): ShortBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Short = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Short): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Short], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Short], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapShortBuffer {
  private[nio] implicit object NewHeapShortBuffer extends GenHeapBuffer.NewHeapBuffer[ShortBuffer, Short] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Short],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): ShortBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapShortBuffer")
      new HeapShortBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(
      array: Array[Short],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): ShortBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
