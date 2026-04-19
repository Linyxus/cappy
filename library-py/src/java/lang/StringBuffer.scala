package java.lang

class StringBuffer private (private val builder: StringBuilder)
    extends AnyRef with CharSequence with Appendable with java.io.Serializable:

  def this() = this(new StringBuilder())
  def this(str: String) = this(new StringBuilder(str))
  def this(capacity: Int) = this(new StringBuilder(capacity))
  def this(seq: CharSequence) = this(new StringBuilder(seq))

  // Every mutation method on StringBuilder returns the builder itself
  // (JVM chain idiom). We evaluate `body` eagerly — typed as its most
  // common return shape `StringBuilder` — and discard the result,
  // returning `this` so the StringBuffer chain matches JVM semantics.
  //
  // Prior experiment with `body: => Any` (by-name) triggered the
  // backend's closure-lowering to mis-bind captured primitives;
  // keep eager-value until that's fixed.
  @inline
  private def withThisResult(body: StringBuilder): this.type =
    this

  def length(): Int = builder.length()

  def capacity(): Int = builder.capacity()

  def ensureCapacity(minimumCapacity: Int): Unit =
    builder.ensureCapacity(minimumCapacity)

  def trimToSize(): Unit =
    builder.trimToSize()

  def setLength(newLength: Int): Unit =
    builder.setLength(newLength)

  def charAt(index: Int): Char =
    builder.charAt(index)

  def codePointAt(index: Int): Int =
    builder.codePointAt(index)

  def codePointBefore(index: Int): Int =
    builder.codePointBefore(index)

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    builder.codePointCount(beginIndex, endIndex)

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    builder.offsetByCodePoints(index, codePointOffset)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
    builder.getChars(srcBegin, srcEnd, dst, dstBegin)

  def setCharAt(index: Int, ch: Char): Unit =
    builder.setCharAt(index, ch)

  def append(obj: AnyRef): StringBuffer =
    withThisResult(builder.append(obj))

  def append(str: String): StringBuffer =
    withThisResult(builder.append(str))

  def append(sb: StringBuffer): StringBuffer =
    withThisResult(builder.append(sb))

  def append(s: CharSequence): StringBuffer =
    withThisResult(builder.append(s))

  def append(s: CharSequence, start: Int, end: Int): StringBuffer =
    withThisResult(builder.append(s, start, end))

  def append(str: Array[Char]): StringBuffer =
    withThisResult(builder.append(str))

  def append(str: Array[Char], offset: Int, count: Int): StringBuffer =
    withThisResult(builder.append(str, offset, count))

  def append(b: scala.Boolean): StringBuffer =
    withThisResult(builder.append(b))

  def append(c: Char): StringBuffer =
    withThisResult(builder.append(c))

  def append(i: Int): StringBuffer =
    withThisResult(builder.append(i))

  def appendCodePoint(codePoint: Int): StringBuffer =
    withThisResult(builder.appendCodePoint(codePoint))

  def append(lng: scala.Long): StringBuffer =
    withThisResult(builder.append(lng))

  def append(f: scala.Float): StringBuffer =
    withThisResult(builder.append(f))

  def append(d: scala.Double): StringBuffer =
    withThisResult(builder.append(d))

  def delete(start: Int, end: Int): StringBuffer =
    withThisResult(builder.delete(start, end))

  def deleteCharAt(index: Int): StringBuffer =
    withThisResult(builder.deleteCharAt(index))

  def replace(start: Int, end: Int, str: String): StringBuffer =
    withThisResult(builder.replace(start, end, str))

  def substring(start: Int): String =
    builder.substring(start)

  def subSequence(start: Int, end: Int): CharSequence =
    builder.subSequence(start, end)

  def substring(start: Int, end: Int): String =
    builder.substring(start, end)

  def insert(index: Int, str: Array[Char], offset: Int, count: Int): StringBuffer =
    withThisResult(builder.insert(index, str, offset, count))

  def insert(offset: Int, obj: AnyRef): StringBuffer =
    withThisResult(builder.insert(offset, obj))

  def insert(offset: Int, str: String): StringBuffer =
    withThisResult(builder.insert(offset, str))

  def insert(offset: Int, str: Array[Char]): StringBuffer =
    withThisResult(builder.insert(offset, str))

  def insert(dstOffset: Int, s: CharSequence): StringBuffer =
    withThisResult(builder.insert(dstOffset, s))

  def insert(dstOffset: Int, s: CharSequence, start: Int, end: Int): StringBuffer =
    withThisResult(builder.insert(dstOffset, s, start, end))

  def insert(offset: Int, b: scala.Boolean): StringBuffer =
    withThisResult(builder.insert(offset, b))

  def insert(offset: Int, c: Char): StringBuffer =
    withThisResult(builder.insert(offset, c))

  def insert(offset: Int, i: Int): StringBuffer =
    withThisResult(builder.insert(offset, i))

  def insert(offset: Int, l: scala.Long): StringBuffer =
    withThisResult(builder.insert(offset, l))

  def insert(offset: Int, f: scala.Float): StringBuffer =
    withThisResult(builder.insert(offset, f))

  def insert(offset: Int, d: scala.Double): StringBuffer =
    withThisResult(builder.insert(offset, d))

  def indexOf(str: String): Int =
    builder.indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    builder.indexOf(str, fromIndex)

  def lastIndexOf(str: String): Int =
    builder.lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    builder.lastIndexOf(str, fromIndex)

  def reverse(): StringBuffer =
    withThisResult(builder.reverse())

  override def toString(): String =
    builder.toString()
