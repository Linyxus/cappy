package java.nio

import scala.language.unsafeNulls

private[nio] final class HeapByteBufferFloatView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean,
    _isDirect: Boolean,
    override private[nio] val isBigEndian: Boolean
) extends FloatBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = _isDirect
  @noinline def slice(): FloatBuffer = {
    val newCapacity = remaining()
    new HeapByteBufferFloatView(newCapacity, _byteArray, _byteArrayOffset + 4 * position(), 0, newCapacity, isReadOnly(), isDirect(), isBigEndian)
  }
  @noinline def duplicate(): FloatBuffer = {
    val result = new HeapByteBufferFloatView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), isReadOnly(), isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): FloatBuffer = {
    val result = new HeapByteBufferFloatView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), true, isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def get(): Float = GenBuffer(this).getElem()
  @noinline def put(c: Float): FloatBuffer = GenBuffer(this).putElem(c)
  @noinline def get(index: Int): Float = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, c: Float): FloatBuffer = GenBuffer(this).putAt(index, c)
  @noinline override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): FloatBuffer = {
    if (isReadOnly())
      throw new ReadOnlyBufferException
    val len = remaining()
    System.arraycopy(_byteArray, _byteArrayOffset + 4 * position(), _byteArray, _byteArrayOffset, 4 * len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }
  @noinline def order(): ByteOrder = if (isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  @inline private def byteArrayBits: ByteArrayBits = ByteArrayBits(_byteArray, _byteArrayOffset, isBigEndian, 4)
  @inline private[nio] def load(index: Int): Float = byteArrayBits.loadFloat(index)
  @inline private[nio] def store(index: Int, elem: Float): Unit = byteArrayBits.storeFloat(index, elem)
}

private[nio] object HeapByteBufferFloatView {
  private[nio] implicit object NewHeapByteBufferFloatView extends GenHeapBufferView.NewHeapBufferView[FloatBuffer] {
    def bytesPerElem: Int = 4
    def apply(
        capacity: Int,
        byteArray: Array[Byte],
        byteArrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        isDirect: Boolean,
        isBigEndian: Boolean
    ): FloatBuffer =
      new HeapByteBufferFloatView(capacity, byteArray, byteArrayOffset, initialPosition, initialLimit, readOnly, isDirect, isBigEndian)
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): FloatBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
