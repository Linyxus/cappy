package java.lang

class StringBuffer private (private val builder: StringBuilder)
    extends AnyRef with CharSequence with Appendable with java.io.Serializable:

  def this() = this(new StringBuilder())
  def this(str: String) = this(new StringBuilder(str))
  def this(capacity: Int) = this(new StringBuilder(capacity))
  def this(seq: CharSequence) = this(new StringBuilder(seq))

  def length(): Int = this.synchronized {
    builder.length()
  }

  def capacity(): Int = this.synchronized {
    builder.capacity()
  }

  def ensureCapacity(minimumCapacity: Int): Unit =
    this.synchronized {
      builder.ensureCapacity(minimumCapacity)
    }

  def trimToSize(): Unit =
    this.synchronized {
      builder.trimToSize()
    }

  def setLength(newLength: Int): Unit =
    this.synchronized {
      builder.setLength(newLength)
    }

  def charAt(index: Int): Char = this.synchronized {
    builder.charAt(index)
  }

  def codePointAt(index: Int): Int = this.synchronized {
    builder.codePointAt(index)
  }

  def codePointBefore(index: Int): Int = this.synchronized {
    builder.codePointBefore(index)
  }

  def codePointCount(beginIndex: Int, endIndex: Int): Int = this.synchronized {
    builder.codePointCount(beginIndex, endIndex)
  }

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int = this.synchronized {
    builder.offsetByCodePoints(index, codePointOffset)
  }

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
    this.synchronized {
      builder.getChars(srcBegin, srcEnd, dst, dstBegin)
    }

  def setCharAt(index: Int, ch: Char): Unit =
    this.synchronized {
      builder.setCharAt(index, ch)
    }

  def append(obj: AnyRef): StringBuffer = this.synchronized {
    builder.append(obj)
    this
  }

  def append(str: String): StringBuffer = this.synchronized {
    builder.append(str)
    this
  }

  def append(sb: StringBuffer): StringBuffer = this.synchronized {
    builder.append(sb)
    this
  }

  def append(s: CharSequence): StringBuffer = this.synchronized {
    builder.append(s)
    this
  }

  def append(s: CharSequence, start: Int, end: Int): StringBuffer = this.synchronized {
    builder.append(s, start, end)
    this
  }

  def append(str: Array[Char]): StringBuffer = this.synchronized {
    builder.append(str)
    this
  }

  def append(str: Array[Char], offset: Int, count: Int): StringBuffer = this.synchronized {
    builder.append(str, offset, count)
    this
  }

  def append(b: scala.Boolean): StringBuffer = this.synchronized {
    builder.append(b)
    this
  }

  def append(c: Char): StringBuffer = this.synchronized {
    builder.append(c)
    this
  }

  def append(i: Int): StringBuffer = this.synchronized {
    builder.append(i)
    this
  }

  def appendCodePoint(codePoint: Int): StringBuffer = this.synchronized {
    builder.appendCodePoint(codePoint)
    this
  }

  def append(lng: scala.Long): StringBuffer = this.synchronized {
    builder.append(lng)
    this
  }

  def append(f: scala.Float): StringBuffer = this.synchronized {
    builder.append(f)
    this
  }

  def append(d: scala.Double): StringBuffer = this.synchronized {
    builder.append(d)
    this
  }

  def delete(start: Int, end: Int): StringBuffer = this.synchronized {
    builder.delete(start, end)
    this
  }

  def deleteCharAt(index: Int): StringBuffer = this.synchronized {
    builder.deleteCharAt(index)
    this
  }

  def replace(start: Int, end: Int, str: String): StringBuffer = this.synchronized {
    builder.replace(start, end, str)
    this
  }

  def substring(start: Int): String = this.synchronized {
    builder.substring(start)
  }

  def subSequence(start: Int, end: Int): CharSequence = this.synchronized {
    builder.subSequence(start, end)
  }

  def substring(start: Int, end: Int): String = this.synchronized {
    builder.substring(start, end)
  }

  def insert(index: Int, str: Array[Char], offset: Int, count: Int): StringBuffer =
    this.synchronized {
      builder.insert(index, str, offset, count)
      this
    }

  def insert(offset: Int, obj: AnyRef): StringBuffer = this.synchronized {
    builder.insert(offset, obj)
    this
  }

  def insert(offset: Int, str: String): StringBuffer = this.synchronized {
    builder.insert(offset, str)
    this
  }

  def insert(offset: Int, str: Array[Char]): StringBuffer = this.synchronized {
    builder.insert(offset, str)
    this
  }

  def insert(dstOffset: Int, s: CharSequence): StringBuffer = this.synchronized {
    builder.insert(dstOffset, s)
    this
  }

  def insert(dstOffset: Int, s: CharSequence, start: Int, end: Int): StringBuffer =
    this.synchronized {
      builder.insert(dstOffset, s, start, end)
      this
    }

  def insert(offset: Int, b: scala.Boolean): StringBuffer = this.synchronized {
    builder.insert(offset, b)
    this
  }

  def insert(offset: Int, c: Char): StringBuffer = this.synchronized {
    builder.insert(offset, c)
    this
  }

  def insert(offset: Int, i: Int): StringBuffer = this.synchronized {
    builder.insert(offset, i)
    this
  }

  def insert(offset: Int, l: scala.Long): StringBuffer = this.synchronized {
    builder.insert(offset, l)
    this
  }

  def insert(offset: Int, f: scala.Float): StringBuffer = this.synchronized {
    builder.insert(offset, f)
    this
  }

  def insert(offset: Int, d: scala.Double): StringBuffer = this.synchronized {
    builder.insert(offset, d)
    this
  }

  def indexOf(str: String): Int = this.synchronized {
    builder.indexOf(str)
  }

  def indexOf(str: String, fromIndex: Int): Int = this.synchronized {
    builder.indexOf(str, fromIndex)
  }

  def lastIndexOf(str: String): Int = this.synchronized {
    builder.lastIndexOf(str)
  }

  def lastIndexOf(str: String, fromIndex: Int): Int = this.synchronized {
    builder.lastIndexOf(str, fromIndex)
  }

  def reverse(): StringBuffer = this.synchronized {
    builder.reverse()
    this
  }

  override def toString(): String = this.synchronized {
    builder.toString()
  }
