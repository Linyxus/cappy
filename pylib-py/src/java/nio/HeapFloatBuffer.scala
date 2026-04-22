package java.nio

private[nio] final class HeapFloatBuffer private (
    _capacity: Int,
    _array0: Array[Float],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends FloatBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): FloatBuffer = {
    val newCapacity = remaining()
    new HeapFloatBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): FloatBuffer = {
    val result = new HeapFloatBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): FloatBuffer = {
    val result = new HeapFloatBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }
  @noinline def get(): Float = GenBuffer(this).getElem()
  @noinline def put(f: Float): FloatBuffer = GenBuffer(this).putElem(f)
  @noinline def get(index: Int): Float = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, f: Float): FloatBuffer = GenBuffer(this).putAt(index, f)
  @noinline override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): FloatBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Float = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Float): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Float], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Float], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapFloatBuffer {
  private[nio] implicit object NewHeapFloatBuffer extends GenHeapBuffer.NewHeapBuffer[FloatBuffer, Float] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Float],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): FloatBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapFloatBuffer")
      new HeapFloatBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(
      array: Array[Float],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): FloatBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
