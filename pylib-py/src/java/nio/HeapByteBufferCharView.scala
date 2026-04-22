package java.nio

import scala.language.unsafeNulls

private[nio] final class HeapByteBufferCharView private (
    _capacity: Int,
    override private[nio] val _byteArray: Array[Byte],
    override private[nio] val _byteArrayOffset: Int,
    _initialPosition: Int,
    _initialLimit: Int,
    _readOnly: Boolean,
    _isDirect: Boolean,
    override private[nio] val isBigEndian: Boolean
) extends CharBuffer(_capacity, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  def isReadOnly(): Boolean = _readOnly
  def isDirect(): Boolean = _isDirect
  @noinline def slice(): CharBuffer = {
    val newCapacity = remaining()
    new HeapByteBufferCharView(newCapacity, _byteArray, _byteArrayOffset + 2 * position(), 0, newCapacity, isReadOnly(), isDirect(), isBigEndian)
  }
  @noinline def duplicate(): CharBuffer = {
    val result = new HeapByteBufferCharView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), isReadOnly(), isDirect(), isBigEndian)
    result._mark = _mark
    result
  }
  @noinline def asReadOnlyBuffer(): CharBuffer = {
    val result = new HeapByteBufferCharView(capacity(), _byteArray, _byteArrayOffset, position(), limit(), true, isDirect(), isBigEndian)
    result._mark = _mark
    result
  }

  def subSequence(start: Int, end: Int): CharBuffer = {
    BoundsChecks.checkStartEnd(start, end, remaining())
    new HeapByteBufferCharView(capacity(), _byteArray, _byteArrayOffset, position() + start, position() + end, isReadOnly(), isDirect(), isBigEndian)
  }

  @noinline def get(): Char = GenBuffer(this).getElem()
  @noinline def put(c: Char): CharBuffer = GenBuffer(this).putElem(c)
  @noinline def get(index: Int): Char = GenBuffer(this).getAt(index)
  @noinline def put(index: Int, c: Char): CharBuffer = GenBuffer(this).putAt(index, c)
  @noinline override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer = GenBuffer(this).getArray(dst, offset, length)
  @noinline override def put(src: Array[Char], offset: Int, length: Int): CharBuffer = GenBuffer(this).putArray(src, offset, length)
  @noinline def compact(): CharBuffer = {
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
  @inline private[nio] def load(index: Int): Char = byteArrayBits.loadChar(index)
  @inline private[nio] def store(index: Int, elem: Char): Unit = byteArrayBits.storeChar(index, elem)
}

private[nio] object HeapByteBufferCharView {
  private[nio] implicit object NewHeapByteBufferCharView extends GenHeapBufferView.NewHeapBufferView[CharBuffer] {
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
    ): CharBuffer =
      new HeapByteBufferCharView(capacity, byteArray, byteArrayOffset, initialPosition, initialLimit, readOnly, isDirect, isBigEndian)
  }

  @inline
  private[nio] def fromHeapByteBuffer(byteBuffer: HeapByteBuffer): CharBuffer =
    GenHeapBufferView.generic_fromHeapByteBuffer(byteBuffer)
}
