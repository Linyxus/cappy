package java.lang

class StringBuilder
    extends AnyRef with CharSequence with Appendable with java.io.Serializable:

  private var buf: Array[Char] = new Array[Char](StringBuilder.DefaultCapacity)
  private var size: Int = 0

  def this(str: String) =
    this()
    if str == null then
      // TODO L4.1: swap to Objects.requireNonNull
      throw new NullPointerException()
    buf = new Array[Char](str.length() + StringBuilder.DefaultCapacity)
    appendKnownString(str)

  def this(initialCapacity: Int) =
    this()
    buf = new Array[Char](initialCapacity) // NegativeArraySizeException check

  def this(seq: CharSequence) =
    this(
      if seq == null then
        throw new NullPointerException()
      else
        seq.toString()
    )

  @inline
  def append(obj: AnyRef): StringBuilder =
    append(if obj == null then "null" else obj.toString())

  @inline
  def append(str: String): StringBuilder =
    appendString(stringOrNullLiteral(str))

  def append(sb: StringBuffer): StringBuilder =
    append(sb: AnyRef)

  def append(s: CharSequence): StringBuilder =
    append(if s == null then "null" else s.toString())

  def append(s: CharSequence, start: Int, end: Int): StringBuilder =
    val s2 = if s == null then "null" else s.toString()
    BoundsChecks.checkStartEnd(start, end, s2.length())
    append(s2.substring(start, end))

  def append(str: Array[Char]): StringBuilder =
    append(String.valueOf(str))

  def append(str: Array[Char], offset: Int, count: Int): StringBuilder =
    BoundsChecks.checkOffsetCount(offset, count, str.length)
    appendChars(str, offset, count)

  def append(b: scala.Boolean): StringBuilder = append(String.valueOf(b))
  def append(c: scala.Char): StringBuilder = append(Character.toString(c))
  def append(i: scala.Int): StringBuilder = append(i.toString())
  def append(lng: scala.Long): StringBuilder = append(lng.toString())
  def append(f: scala.Float): StringBuilder = append(f.toString())
  def append(d: scala.Double): StringBuilder = append(d.toString())

  def appendCodePoint(codePoint: Int): StringBuilder =
    append(Character.toString(codePoint))

  def delete(start: Int, end: Int): StringBuilder =
    replace(start, end, "")

  def deleteCharAt(index: Int): StringBuilder =
    deleteRange(checkIndex(index), index + 1)

  def replace(start: Int, end: Int, str: String): StringBuilder =
    if str == null then
      // TODO L4.1: swap to Objects.requireNonNull
      throw new NullPointerException()
    val checkedEnd = checkReplaceEnd(start, end)
    replaceRange(start, checkedEnd, str)

  def insert(index: Int, str: Array[scala.Char], offset: Int, count: Int): StringBuilder =
    insert(index, String.valueOf(str, offset, count))

  @inline
  def insert(offset: Int, obj: AnyRef): StringBuilder =
    insert(offset, String.valueOf(obj))

  def insert(offset: Int, str: String): StringBuilder =
    insertString(offset, stringOrNullLiteral(str))

  def insert(offset: Int, str: Array[scala.Char]): StringBuilder =
    insert(offset, String.valueOf(str))

  def insert(dstOffset: Int, s: CharSequence): StringBuilder =
    checkInsertOffset(dstOffset)
    val s2 = if s == null then "null" else s.toString()
    insertCharsAt(dstOffset, s2)

  def insert(dstOffset: Int, s: CharSequence, start: Int, end: Int): StringBuilder =
    checkInsertOffset(dstOffset)
    val s2 = if s == null then "null" else s.toString()
    BoundsChecks.checkStartEnd(start, end, s2.length())
    insertCharsAt(dstOffset, s2.substring(start, end))

  def insert(offset: Int, b: scala.Boolean): StringBuilder =
    insert(offset, String.valueOf(b))

  def insert(offset: Int, c: scala.Char): StringBuilder =
    checkInsertOffset(offset)
    insertCharsAt(offset, Character.toString(c))

  def insert(offset: Int, i: scala.Int): StringBuilder =
    insert(offset, i.toString())

  def insert(offset: Int, l: scala.Long): StringBuilder =
    insert(offset, l.toString())

  def insert(offset: Int, f: scala.Float): StringBuilder =
    insert(offset, f.toString())

  def insert(offset: Int, d: scala.Double): StringBuilder =
    insert(offset, d.toString())

  def indexOf(str: String): Int = toString().indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    toString().indexOf(str, fromIndex)

  def lastIndexOf(str: String): Int = toString().lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    toString().lastIndexOf(str, fromIndex)

  def reverse(): StringBuilder =
    // O(n) in-place reverse into a fresh buffer. Walks `buf` from the
    // back and writes to `newBuf` from the front, preserving
    // high+low-surrogate pairs (the pair must stay in original order
    // because the two halves together encode one code point). Replaces
    // the earlier O(n²) `var result = ""; result += ...` loop.
    val newBuf = new Array[Char](size)
    var srcIdx = size - 1
    var dstIdx = 0
    while srcIdx >= 0 do
      val c = buf(srcIdx)
      if srcIdx > 0 &&
          Character.isLowSurrogate(c) &&
          Character.isHighSurrogate(buf(srcIdx - 1)) then
        newBuf(dstIdx)     = buf(srcIdx - 1)
        newBuf(dstIdx + 1) = c
        dstIdx += 2
        srcIdx -= 2
      else
        newBuf(dstIdx) = c
        dstIdx += 1
        srcIdx -= 1
    buf = newBuf
    // size is unchanged: same element count, different order.
    this

  override def toString(): String =
    new String(buf, 0, size)

  def length(): Int = size

  def capacity(): Int = buf.length

  def ensureCapacity(minimumCapacity: Int): Unit =
    if minimumCapacity > 0 then
      ensureCapacityInternal(minimumCapacity)

  def trimToSize(): Unit =
    if size < buf.length then
      val trimmed = new Array[Char](size)
      copyChars(buf, 0, trimmed, 0, size)
      buf = trimmed

  def setLength(newLength: Int): Unit =
    if newLength < 0 then
      throw new StringIndexOutOfBoundsException(newLength)
    ensureCapacityInternal(newLength)
    if newLength > size then
      var i = size
      while i < newLength do
        buf(i) = 0.toChar
        i += 1
    size = newLength

  def charAt(index: Int): Char =
    buf(checkIndex(index))

  def codePointAt(index: Int): Int =
    toString().codePointAt(index)

  def codePointBefore(index: Int): Int =
    toString().codePointBefore(index)

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    toString().codePointCount(beginIndex, endIndex)

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    toString().offsetByCodePoints(index, codePointOffset)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[scala.Char], dstBegin: Int): Unit =
    val count = checkStartEnd(srcBegin, srcEnd)
    BoundsChecks.checkOffsetCount(dstBegin, count, dst.length)
    copyChars(buf, srcBegin, dst, dstBegin, count)

  def setCharAt(index: Int, ch: scala.Char): Unit =
    buf(checkIndex(index)) = ch

  def substring(start: Int): String =
    substring(start, size)

  def subSequence(start: Int, end: Int): CharSequence =
    substring(start, end)

  def substring(start: Int, end: Int): String =
    val count = checkStartEnd(start, end)
    new String(buf, start, count)

  private def appendString(str: String): StringBuilder =
    appendKnownString(str)
    this

  private def appendKnownString(str: String): Unit =
    val strLength = str.length()
    ensureCapacityInternal(size + strLength)
    var i = 0
    while i < strLength do
      buf(size + i) = str.charAt(i)
      i += 1
    size += strLength

  private def appendChars(chars: Array[Char], offset: Int, count: Int): StringBuilder =
    ensureCapacityInternal(size + count)
    copyChars(chars, offset, buf, size, count)
    size += count
    this

  private def insertString(offset: Int, str: String): StringBuilder =
    checkIndexInclusive(offset)
    insertCharsAt(offset, str)

  private def insertCharsAt(offset: Int, str: String): StringBuilder =
    val strLength = str.length()
    ensureCapacityInternal(size + strLength)
    moveChars(offset, offset + strLength, size - offset)
    var i = 0
    while i < strLength do
      buf(offset + i) = str.charAt(i)
      i += 1
    size += strLength
    this

  private def replaceRange(start: Int, end: Int, str: String): StringBuilder =
    val replacementLength = str.length()
    val removedLength = end - start
    val newSize = size - removedLength + replacementLength
    ensureCapacityInternal(newSize)
    moveChars(end, start + replacementLength, size - end)
    var i = 0
    while i < replacementLength do
      buf(start + i) = str.charAt(i)
      i += 1
    size = newSize
    this

  private def deleteRange(start: Int, end: Int): StringBuilder =
    moveChars(end, start, size - end)
    size -= (end - start)
    this

  private def moveChars(srcPos: Int, dstPos: Int, count: Int): Unit =
    copyChars(buf, srcPos, buf, dstPos, count)

  private def ensureCapacityInternal(min: Int): Unit =
    if min > buf.length then
      growCapacity(min)

  private def growCapacity(min: Int): Unit =
    val doubled =
      if buf.length == 0 then StringBuilder.DefaultCapacity
      else buf.length * 2
    val newCapacity =
      if doubled < min || doubled < 0 then min
      else doubled
    val grown = new Array[Char](newCapacity)
    copyChars(buf, 0, grown, 0, size)
    buf = grown

  private def stringOrNullLiteral(str: String): String =
    if str == null then "null" else str

  private def checkIndex(index: Int): Int =
    if index < 0 || index >= size then
      throw new StringIndexOutOfBoundsException(index)
    index

  private def checkIndexInclusive(index: Int): Int =
    if index < 0 || index > size then
      throw new StringIndexOutOfBoundsException(index)
    index

  private def checkInsertOffset(offset: Int): Unit =
    BoundsChecks.checkIndexInclusive(offset, size)

  private def checkStartEnd(start: Int, end: Int): Int =
    if start < 0 || end < start || end > size then
      throw new StringIndexOutOfBoundsException()
    end - start

  private def checkReplaceEnd(start: Int, end: Int): Int =
    checkIndexInclusive(start)
    if end < start then
      throw new StringIndexOutOfBoundsException()
    if end > size then size else end

  private def copyChars(
      src: Array[Char],
      srcPos: Int,
      dst: Array[Char],
      dstPos: Int,
      count: Int
  ): Unit =
    if count > 0 then
      if src.eq(dst) && dstPos > srcPos then
        var i = count - 1
        while i >= 0 do
          dst(dstPos + i) = src(srcPos + i)
          i -= 1
      else
        var i = 0
        while i < count do
          dst(dstPos + i) = src(srcPos + i)
          i += 1

object StringBuilder:
  private final val DefaultCapacity = 16
