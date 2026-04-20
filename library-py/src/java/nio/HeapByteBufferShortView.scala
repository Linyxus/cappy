package java.nio

import scala.language.unsafeNulls

private[nio] final class HeapByteBufferShortView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean,
    _isDirect: Boolean,
    override private[nio] val isBigEndian: Boolean
) extends ShortBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = _isDirect
  @noinline def slice(): ShortBuffer = {
    val newCapacity = remaining()
    new HeapByteBufferShortView(newCapacity, _byteArray, _byteArrayOffset + 2 * position(), 0, newCapacity, isReadOnly(), isDirect(), isBigEndian)
  }
  @noinline def duplicate(): ShortBuffer = {
    val result = new HeapByteBufferShortView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), isReadOnly(), isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): ShortBuffer = {
    val result = new HeapByteBufferShortView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), true, isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def get(): Short = GenBuffer(this).getElem()
  @noinline def put(c: Short): ShortBuffer = GenBuffer(this).putElem(c)
  @noinline def get(index: Int): Short = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, c: Short): ShortBuffer = GenBuffer(this).putAt(index, c)
  @noinline override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): ShortBuffer = {
    if (isReadOnly())
      throw new ReadOnlyBufferException
    val len = remaining()
    System.arraycopy(_byteArray, _byteArrayOffset + 2 * position(), _byteArray, _byteArrayOffset, 2 * len)
    _mark = -1
    limit(capacity())
    position(len)
    this
  }
  @noinline def order(): ByteOrder = if (isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  @inline private def byteArrayBits: ByteArrayBits = ByteArrayBits(_byteArray, _byteArrayOffset, isBigEndian, 2)
  @inline private[nio] def load(index: Int): Short = byteArrayBits.loadShort(index)
  @inline private[nio] def store(index: Int, elem: Short): Unit = byteArrayBits.storeShort(index, elem)
}

private[nio] object HeapByteBufferShortView {
  private[nio] implicit object NewHeapByteBufferShortView extends GenHeapBufferView.NewHeapBufferView[ShortBuffer] {
    def bytesPerElem: Int = 2
    def apply(
        capacity: Int,
        byteArray: Array[Byte],
        byteArrayOffset: Int,
        initialPosition: Int,
        initialLimit: Int,
        readOnly: Boolean,
        isDirect: Boolean,
        isBigEndian: Boolean
    ): ShortBuffer =
      new HeapByteBufferShortView(capacity, byteArray, byteArrayOffset, initialPosition, initialLimit, readOnly, isDirect, isBigEndian)
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): ShortBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
