package java.nio

private[nio] final class HeapIntBuffer private (
    _capacity: Int,
    _array0: Array[Int],
    _arrayOffset0: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean
) extends IntBuffer(_capacity, _array0, _arrayOffset0) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = false

  @noinline def slice(): IntBuffer = {
    val newCapacity = remaining()
    new HeapIntBuffer(newCapacity, _array, _arrayOffset + position(), 0, newCapacity, isReadOnly())
  }
  @noinline def duplicate(): IntBuffer = {
    val result = new HeapIntBuffer(capacity(), _array, _arrayOffset, position(), limit(), isReadOnly())
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): IntBuffer = {
    val result = new HeapIntBuffer(capacity(), _array, _arrayOffset, position(), limit(), true)
    result._mark = _mark
    result
  }
  @noinline def get(): Int = GenBuffer(this).getElem()
  @noinline def put(i: Int): IntBuffer = GenBuffer(this).putElem(i)
  @noinline def get(index: Int): Int = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, i: Int): IntBuffer = GenBuffer(this).putAt(index, i)
  @noinline override def get(dst: Array[Int], offset: Int, length: Int): IntBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Int], offset: Int, length: Int): IntBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): IntBuffer = {
    ensureNotReadOnly()
    val len = remaining()
    System.arraycopy(_array, _arrayOffset + position(), _array, _arrayOffset, len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }

  def order(): ByteOrder = ByteOrder.nativeOrder()

  @inline private[nio] def load(index: Int): Int = _array(_arrayOffset + index)
  @inline private[nio] def store(index: Int, elem: Int): Unit = _array(_arrayOffset + index) = elem
  @inline override private[nio] def load(startIndex: Int, dst: Array[Int], offset: Int, length: Int): Unit =
    System.arraycopy(_array, _arrayOffset + startIndex, dst, offset, length)
  @inline override private[nio] def store(startIndex: Int, src: Array[Int], offset: Int, length: Int): Unit =
    System.arraycopy(src, offset, _array, _arrayOffset + startIndex, length)
}

private[nio] object HeapIntBuffer {
  private[nio] implicit object NewHeapIntBuffer extends GenHeapBuffer.NewHeapBuffer[IntBuffer, Int] {
    @inline
    def apply(
        capacity: Int,
        array: Array[Int],
        arrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        direct: Boolean
    ): IntBuffer = {
      if (direct)
        throw new AssertionError("Cannot create a direct HeapIntBuffer")
      new HeapIntBuffer(capacity, array, arrayOffset, initialPosition, initialLimit, readOnly)
    }
  }

  @noinline
  private[nio] def wrap(
      array: Array[Int],
      arrayOffset: Int,
      capacity: Int,
      initialPosition: Int,
      initialLength: Int,
      isReadOnly: Boolean
  ): IntBuffer =
    GenHeapBuffer.generic_wrap(array, arrayOffset, capacity, initialPosition, initialLength, isReadOnly)
}
