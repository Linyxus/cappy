package java.nio

import scala.language.unsafeNulls

private[nio] final class HeapByteBufferDoubleView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean,
    _isDirect: Boolean,
    override private[nio] val isBigEndian: Boolean
) extends DoubleBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = _isDirect
  @noinline def slice(): DoubleBuffer = {
    val newCapacity = remaining()
    new HeapByteBufferDoubleView(newCapacity, _byteArray, _byteArrayOffset + 8 * position(), 0, newCapacity, isReadOnly(), isDirect(), isBigEndian)
  }
  @noinline def duplicate(): DoubleBuffer = {
    val result = new HeapByteBufferDoubleView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), isReadOnly(), isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): DoubleBuffer = {
    val result = new HeapByteBufferDoubleView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), true, isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def get(): Double = GenBuffer(this).getElem()
  @noinline def put(c: Double): DoubleBuffer = GenBuffer(this).putElem(c)
  @noinline def get(index: Int): Double = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, c: Double): DoubleBuffer = GenBuffer(this).putAt(index, c)
  @noinline override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): DoubleBuffer = {
    if (isReadOnly())
      throw new ReadOnlyBufferException
    val len = remaining()
    System.arraycopy(_byteArray, _byteArrayOffset + 8 * position(), _byteArray, _byteArrayOffset, 8 * len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }
  @noinline def order(): ByteOrder = if (isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  @inline private def byteArrayBits: ByteArrayBits = ByteArrayBits(_byteArray, _byteArrayOffset, isBigEndian, 8)
  @inline private[nio] def load(index: Int): Double = byteArrayBits.loadDouble(index)
  @inline private[nio] def store(index: Int, elem: Double): Unit = byteArrayBits.storeDouble(index, elem)
}

private[nio] object HeapByteBufferDoubleView {
  private[nio] implicit object NewHeapByteBufferDoubleView extends GenHeapBufferView.NewHeapBufferView[DoubleBuffer] {
    def bytesPerElem: Int = 8
    def apply(
        capacity: Int,
        byteArray: Array[Byte],
        byteArrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        isDirect: Boolean,
        isBigEndian: Boolean
    ): DoubleBuffer =
      new HeapByteBufferDoubleView(capacity, byteArray, byteArrayOffset, initialPosition, initialLimit, readOnly, isDirect, isBigEndian)
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): DoubleBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
