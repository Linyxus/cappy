/*
 * Port of scala-js javalib Long, adapted for the ScalaPy backend.
 *
 * Python represents 64-bit integer values as native `int`, so this port
 * keeps the Scala.js public surface but drops the JS-specific RuntimeLong
 * branches and uses the scalar 64-bit algorithms throughout.
 */

package java.lang

import scala.annotation.switch

import java.lang.constant.{Constable, ConstantDesc}

final class Long private () extends Number with Comparable[Long] with Constable with ConstantDesc {

  def this(value: scala.Long) = this()
  def this(s: String) = this()

  @inline def longValue(): scala.Long =
    this.asInstanceOf[scala.Long]

  @inline override def byteValue(): scala.Byte = longValue().toByte
  @inline override def shortValue(): scala.Short = longValue().toShort
  @inline def intValue(): scala.Int = longValue().toInt
  @inline def floatValue(): scala.Float = longValue().toFloat
  @inline def doubleValue(): scala.Double = longValue().toDouble

  @inline override def equals(that: Any): scala.Boolean = that match {
    case that: Long => longValue() == that.longValue()
    case _          => false
  }

  @inline override def hashCode(): Int =
    Long.hashCode(longValue())

  @inline override def compareTo(that: Long): Int =
    Long.compare(longValue(), that.longValue())

  @inline override def toString(): String =
    Long.toString(longValue())

}

object Long {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Long]

  final val MIN_VALUE = -9223372036854775808L
  final val MAX_VALUE = 9223372036854775807L
  final val SIZE = 64
  final val BYTES = 8

  private final val SignBit = scala.Long.MinValue

  @inline private def isRadixInvalid(radix: Int): scala.Boolean =
    radix < 2 || radix > 36

  @inline
  def toString(i: scala.Long, radix: Int): String = {
    if (radix == 10 || isRadixInvalid(radix))
      toString(i)
    else {
      val negative = i < 0L
      val abs = Math.abs(i)
      toStringGeneric(abs, radix, negative)
    }
  }

  @inline
  def toUnsignedString(i: scala.Long, radix: Int): String = {
    (radix: @switch) match {
      case 2  => toBinaryString(i)
      case 8  => toOctalString(i)
      case 16 => toHexString(i)
      case _  =>
        val radix1 =
          if (isRadixInvalid(radix)) 10
          else radix
        toStringGeneric(i, radix1, negative = false)
    }
  }

  @inline def toString(i: scala.Long): String = "" + i

  @inline def toUnsignedString(i: scala.Long): String =
    if (i >= 0L) toString(i)
    else {
      val quotient = (i >>> 1) / 5L
      val remainder = i - quotient * 10L
      toString(quotient) + remainder
    }

  private def toStringGeneric(value0: scala.Long, radix: scala.Int, negative: scala.Boolean): String = {
    if (value0 == 0L) {
      "0"
    } else {
      val maxChars = 65 // sign + 64 binary digits
      val buffer = new Array[Char](maxChars)
      var pos = maxChars - 1
      var value = value0
      val longRadix = radix.toLong

      while (value != 0L) {
        val nextValue = Long.divideUnsigned(value, longRadix)
        val digit = (value - longRadix * nextValue).toInt
        buffer(pos) = ((if (digit < 10) '0'.toInt else 'a'.toInt - 10) + digit).toChar
        pos -= 1
        value = nextValue
      }

      if (negative) {
        buffer(pos) = '-'
        pos -= 1
      }

      var result = ""
      var i = pos + 1
      while (i < maxChars) {
        result += buffer(i)
        i += 1
      }
      result
    }
  }

  private def parseLongFail(s: String): Nothing =
    Integer.parseIntFail(s)

  @inline def parseLong(s: String): scala.Long =
    parseLong(s, 10)

  @inline
  def parseLong(s: String, radix: Int): scala.Long = {
    if (isRadixInvalid(radix))
      parseLongFail(s)
    IntegerLong.parseSignedImpl(s, radix, divideUnsigned(MIN_VALUE, radix.toLong))
  }

  @inline def parseUnsignedLong(s: String): scala.Long =
    parseUnsignedLong(s, 10)

  @inline
  def parseUnsignedLong(s: String, radix: Int): scala.Long = {
    if (isRadixInvalid(radix))
      parseLongFail(s)
    IntegerLong.parseUnsignedImpl(s, radix, divideUnsigned(-1L, radix.toLong))
  }

  @inline def `new`(value: scala.Long): Long = valueOf(value)

  @inline def `new`(s: String): Long = valueOf(s)

  @inline def valueOf(l: scala.Long): Long = l.asInstanceOf[Long]

  @inline def valueOf(s: String): Long = valueOf(parseLong(s))

  @inline def valueOf(s: String, radix: Int): Long =
    valueOf(parseLong(s, radix))

  @noinline def decode(nm: String): Long =
    Integer.decodeGeneric(nm, valueOf(_, _))

  @inline def hashCode(value: scala.Long): Int =
    value.toInt ^ (value >>> 32).toInt

  @inline def compare(x: scala.Long, y: scala.Long): scala.Int = {
    if (x == y) 0
    else if (x < y) -1
    else 1
  }

  @inline def compareUnsigned(x: scala.Long, y: scala.Long): scala.Int = {
    if (x == y) 0
    else if (unsigned_<(x, y)) -1
    else 1
  }

  @inline private[java] def unsigned_<(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) < (y ^ SignBit)

  @inline private[java] def unsigned_<=(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) <= (y ^ SignBit)

  @inline private[java] def unsigned_>(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) > (y ^ SignBit)

  @inline private[java] def unsigned_>=(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) >= (y ^ SignBit)

  /** JDK-style unsigned division without BigInteger. */
  @inline def divideUnsigned(dividend: scala.Long, divisor: scala.Long): scala.Long =
    if (divisor < 0L) {
      if (unsigned_<(dividend, divisor)) 0L else 1L
    } else if (dividend >= 0L) {
      dividend / divisor
    } else {
      val quotient = ((dividend >>> 1) / divisor) << 1
      val rem = dividend - quotient * divisor
      quotient + (if (unsigned_>=(rem, divisor)) 1L else 0L)
    }

  @inline def remainderUnsigned(dividend: scala.Long, divisor: scala.Long): scala.Long =
    dividend - divideUnsigned(dividend, divisor) * divisor

  @inline
  def highestOneBit(i: scala.Long): scala.Long =
    if (i == 0L) 0L
    else SignBit >>> numberOfLeadingZeros(i)

  @inline
  def lowestOneBit(i: scala.Long): scala.Long =
    i & -i

  @inline
  def bitCount(i: scala.Long): scala.Int = {
    val lo = i.toInt
    val hi = (i >>> 32).toInt
    Integer.bitCount(lo) + Integer.bitCount(hi)
  }

  @inline
  def reverseBytes(i: scala.Long): scala.Long =
    makeLongFromLoHi(
      Integer.reverseBytes((i >>> 32).toInt),
      Integer.reverseBytes(i.toInt)
    )

  @inline
  def reverse(i: scala.Long): scala.Long =
    makeLongFromLoHi(
      Integer.reverse((i >>> 32).toInt),
      Integer.reverse(i.toInt)
    )

  @inline
  private def makeLongFromLoHi(lo: Int, hi: Int): scala.Long =
    (lo.toLong & 0xffffffffL) | (hi.toLong << 32)

  @inline
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long =
    val shift = distance & 63
    if shift == 0 then i
    else (i << shift) | (i >>> (64 - shift))

  @inline
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long =
    val shift = distance & 63
    if shift == 0 then i
    else (i >>> shift) | (i << (64 - shift))

  @noinline
  def compress(i: scala.Long, mask: scala.Long): scala.Long =
    IntegerLong.compress(i, mask)

  @noinline
  def expand(i: scala.Long, mask: scala.Long): scala.Long =
    IntegerLong.expand(i, mask)

  @inline
  def signum(i: scala.Long): Int =
    ((i >> 63) | (-i >>> 63)).toInt

  @inline
  def numberOfLeadingZeros(l: scala.Long): Int = {
    val hi = (l >>> 32).toInt
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(l.toInt) + 32
  }

  @inline
  def numberOfTrailingZeros(l: scala.Long): Int = {
    val lo = l.toInt
    if (lo != 0) Integer.numberOfTrailingZeros(lo)
    else Integer.numberOfTrailingZeros((l >>> 32).toInt) + 32
  }

  @inline def toBinaryString(l: scala.Long): String =
    toBinaryString(l.toInt, (l >>> 32).toInt)

  private def toBinaryString(lo: Int, hi: Int): String = {
    val zeros = "00000000000000000000000000000000"
    @inline def padBinary32(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }

    if (hi != 0) Integer.toBinaryString(hi) + padBinary32(lo)
    else Integer.toBinaryString(lo)
  }

  @inline def toHexString(l: scala.Long): String =
    toHexString(l.toInt, (l >>> 32).toInt)

  private def toHexString(lo: Int, hi: Int): String = {
    val zeros = "00000000"
    @inline def padHex8(i: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length) + s
    }

    if (hi != 0) Integer.toHexString(hi) + padHex8(lo)
    else Integer.toHexString(lo)
  }

  @inline def toOctalString(l: scala.Long): String =
    toOctalString(l.toInt, (l >>> 32).toInt)

  private def toOctalString(lo: Int, hi: Int): String = {
    val zeros = "0000000000"
    @inline def padOctal10(i: Int) = {
      val s = Integer.toOctalString(i)
      zeros.substring(s.length) + s
    }

    val lp = lo & 0x3fffffff
    val mp = ((lo >>> 30) + (hi << 2)) & 0x3fffffff
    val hp = hi >>> 28

    if (hp != 0) Integer.toOctalString(hp) + padOctal10(mp) + padOctal10(lp)
    else if (mp != 0) Integer.toOctalString(mp) + padOctal10(lp)
    else Integer.toOctalString(lp)
  }

  @inline def sum(a: scala.Long, b: scala.Long): scala.Long =
    a + b

  @inline def max(a: scala.Long, b: scala.Long): scala.Long =
    Math.max(a, b)

  @inline def min(a: scala.Long, b: scala.Long): scala.Long =
    Math.min(a, b)
}
