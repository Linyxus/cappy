package java.nio

private[nio] final class HeapDoubleBuffer private (
    _capacity: Int,
    _array0: Array[Double],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends DoubleBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): DoubleBuffer = {
    val newCapacity = remaining()
    new HeapDoubleBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): DoubleBuffer = {
    val result = new HeapDoubleBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): DoubleBuffer = {
    val result = new HeapDoubleBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }
  @noinline def get(): Double = GenBuffer(this).getElem()
  @noinline def put(d: Double): DoubleBuffer = GenBuffer(this).putElem(d)
  @noinline def get(index: Int): Double = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, d: Double): DoubleBuffer = GenBuffer(this).putAt(index, d)
  @noinline override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): DoubleBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Double = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Double): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Double], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Double], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapDoubleBuffer {
  private[nio] implicit object NewHeapDoubleBuffer extends GenHeapBuffer.NewHeapBuffer[DoubleBuffer, Double] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Double],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): DoubleBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapDoubleBuffer")
      new HeapDoubleBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(
      array: Array[Double],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): DoubleBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
