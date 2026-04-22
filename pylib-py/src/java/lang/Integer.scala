/*
 * Port of scala-js javalib Integer, adapted for the ScalaPy backend.
 * See notes/javalib-plan.md (unit L1.1).
 *
 * Scala.js adaptations:
 * - Dropped `scala.scalajs.js.*` / `LinkingInfo` imports.
 * - Radix formatting via a Scala loop instead of JS's
 *   `Number.prototype.toString(radix)`.
 * - `divideUnsigned` / `remainderUnsigned` / `toUnsignedLong` /
 *   `intBitsToFloat` / `floatToRawIntBits` / `numberOfLeadingZeros`
 *   are backend stubs in scala-js; on the Python backend we implement
 *   them directly (Python `int` is arbitrary-precision, so masking
 *   gives the unsigned view).
 * - Parsing shares `IntegerLong.parseSignedImpl` / `parseUnsignedImpl`
 *   with `Long` via the `IntFloatBits[Int, Float]` typeclass.
 */

package java.lang

import java.lang.constant.{Constable, ConstantDesc}
import java.util.function._

final class Integer private ()
    extends Number with Comparable[Integer] with Constable with ConstantDesc:

  def this(value: scala.Int) = this()
  def this(s: String) = this()

  def intValue(): scala.Int =
    this.asInstanceOf[scala.Int]

  override def byteValue(): scala.Byte = intValue().toByte
  override def shortValue(): scala.Short = intValue().toShort
  def longValue(): scala.Long = intValue().toLong
  def floatValue(): scala.Float = intValue().toFloat
  def doubleValue(): scala.Double = intValue().toDouble

  override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  override def hashCode(): Int = intValue()

  override def compareTo(that: Integer): Int =
    Integer.compare(intValue(), that.intValue())

  override def toString(): String =
    Integer.toString(intValue())

object Integer:
  val TYPE: Class[?] = scala.Predef.classOf[scala.Int]

  final val MIN_VALUE = -2147483648
  final val MAX_VALUE = 2147483647
  final val SIZE = 32
  final val BYTES = 4

  private final val SignBit = Int.MinValue

  def valueOf(i: scala.Int): Integer = i.asInstanceOf[Integer]

  def valueOf(s: String): Integer = valueOf(parseInt(s))

  def valueOf(s: String, radix: Int): Integer =
    valueOf(parseInt(s, radix))

  def parseIntFail(s: String): Nothing =
    throw new NumberFormatException(s"""For input string: "$s"""")

  def parseInt(s: String): scala.Int = parseInt(s, 10)

  def parseInt(s: String, radix: scala.Int): scala.Int =
    if radix < 2 || radix > 36 then parseIntFail(s)
    else IntegerLong.parseSignedImpl(s, radix, divideUnsigned(MIN_VALUE, radix))

  def parseUnsignedInt(s: String): scala.Int = parseUnsignedInt(s, 10)

  def parseUnsignedInt(s: String, radix: scala.Int): scala.Int =
    if radix < 2 || radix > 36 then parseIntFail(s)
    else IntegerLong.parseUnsignedImpl(s, radix, divideUnsigned(-1, radix))

  def toString(i: scala.Int): String = "" + i

  def toUnsignedString(i: Int, radix: Int): String =
    toStringBase(i, radix)

  def decode(nm: String): Integer =
    decodeGeneric(nm, (s, r) => valueOf(s, r))

  def decodeGeneric[A](nm: String, parse: BiFunction[String, Int, A]): A =
    val inputLength = nm.length()
    var i = 0

    val negative =
      if i != inputLength then
        val ch = nm.charAt(i)
        if ch == '+' then
          i += 1
          false
        else if ch == '-' then
          i += 1
          true
        else
          false
      else false

    val base =
      if i != inputLength then
        val ch = nm.charAt(i)
        if ch == '0' then
          if i == inputLength - 1 then 10
          else
            i += 1
            val radixPrefix = nm.charAt(i)
            if radixPrefix == 'x' || radixPrefix == 'X' then
              i += 1
              16
            else
              8
        else if ch == '#' then
          i += 1
          16
        else
          10
      else 10

    val remaining = nm.substring(i)
    if remaining.startsWith("+") || remaining.startsWith("-") then
      throw new NumberFormatException("Sign character in wrong position")

    val s = if negative then "-" + remaining else remaining
    parse.apply(s, base)

  def compare(x: scala.Int, y: scala.Int): scala.Int =
    if x == y then 0
    else if x < y then -1
    else 1

  def compareUnsigned(x: scala.Int, y: scala.Int): scala.Int =
    if x == y then 0
    else if unsigned_<(x, y) then -1
    else 1

  def unsigned_<(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) < (y ^ SignBit)

  def unsigned_<=(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) <= (y ^ SignBit)

  def unsigned_>(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) > (y ^ SignBit)

  def unsigned_>=(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) >= (y ^ SignBit)

  def toUnsignedLong(x: Int): scala.Long =
    x.toLong & 0xFFFFFFFFL

  def toUnsignedDouble(x: Int): scala.Double =
    toUnsignedLong(x).toDouble

  def bitCount(i: scala.Int): scala.Int =
    val t1 = i - ((i >> 1) & 0x55555555)
    val t2 = (t1 & 0x33333333) + ((t1 >> 2) & 0x33333333)
    (((t2 + (t2 >> 4)) & 0xf0f0f0f) * 0x1010101) >> 24

  def divideUnsigned(dividend: Int, divisor: Int): Int =
    val du = dividend.toLong & 0xFFFFFFFFL
    val dv = divisor.toLong & 0xFFFFFFFFL
    (du / dv).toInt

  def remainderUnsigned(dividend: Int, divisor: Int): Int =
    val du = dividend.toLong & 0xFFFFFFFFL
    val dv = divisor.toLong & 0xFFFFFFFFL
    (du % dv).toInt

  def highestOneBit(i: Int): Int =
    if i == 0 then 0
    else 1 << (31 - numberOfLeadingZeros(i))

  def lowestOneBit(i: Int): Int = i & -i

  def reverseBytes(i: scala.Int): scala.Int =
    rotateRight(i & 0x00ff00ff, 8) | (rotateLeft(i, 8) & 0x00ff00ff)

  def reverse(i: scala.Int): scala.Int =
    val x0 = rotateLeft(i, 15)
    val t1 = (x0 ^ (x0 >> 10)) & 0x003f801f
    val x1 = (t1 | (t1 << 10)) ^ x0
    val t2 = (x1 ^ (x1 >> 4)) & 0x0e038421
    val x2 = (t2 | (t2 << 4)) ^ x1
    val t3 = (x2 ^ (x2 >> 2)) & 0x22488842
    (t3 | (t3 << 2)) ^ x2

  def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int =
    val shift = distance & 31
    if shift == 0 then i
    else (i << shift) | (i >>> (32 - shift))

  def rotateRight(i: scala.Int, distance: scala.Int): scala.Int =
    val shift = distance & 31
    if shift == 0 then i
    else (i >>> shift) | (i << (32 - shift))

  def signum(i: scala.Int): scala.Int =
    (i >> 31) | (-i >>> 31)

  def numberOfLeadingZeros(i: scala.Int): scala.Int =
    if i == 0 then 32
    else
      var n = 0
      var x = i
      if (x & 0xFFFF0000) == 0 then { n += 16; x <<= 16 }
      if (x & 0xFF000000) == 0 then { n += 8;  x <<= 8 }
      if (x & 0xF0000000) == 0 then { n += 4;  x <<= 4 }
      if (x & 0xC0000000) == 0 then { n += 2;  x <<= 2 }
      if (x & 0x80000000) == 0 then n += 1
      n

  def numberOfTrailingZeros(i: scala.Int): scala.Int =
    32 - numberOfLeadingZeros(~i & (i - 1))

  def toBinaryString(i: scala.Int): String = toStringBase(i, 2)
  def toHexString(i: scala.Int): String = toStringBase(i, 16)
  def toOctalString(i: scala.Int): String = toStringBase(i, 8)

  def toString(i: Int, radix: Int): String =
    if radix == 10 || radix < 2 || radix > 36 then
      Integer.toString(i)
    else
      if i == 0 then "0"
      else if i < 0 then "-" + toUnsignedStringBase(-i.toLong & 0xFFFFFFFFL, radix)
      else toUnsignedStringBase(i.toLong, radix)

  def toUnsignedString(i: scala.Int): String = toUnsignedString(i, 10)

  def hashCode(value: Int): Int = value

  def sum(a: Int, b: Int): Int = a + b
  def max(a: Int, b: Int): Int = if a >= b then a else b
  def min(a: Int, b: Int): Int = if a <= b then a else b

  private def toUnsignedStringBase(value: scala.Long, radix: Int): String =
    if value == 0L then "0"
    else
      val buf = new Array[Char](33)
      var pos = 32
      var v = value
      val r = radix.toLong
      while v != 0L do
        val digit = (v % r).toInt
        buf(pos) = Character.forDigit(digit, radix)
        pos -= 1
        v = v / r
      var out = ""
      var idx = pos + 1
      while idx <= 32 do
        out += buf(idx)
        idx += 1
      out

  private def toStringBase(i: scala.Int, base: scala.Int): String =
    toUnsignedStringBase(toUnsignedLong(i), base)
