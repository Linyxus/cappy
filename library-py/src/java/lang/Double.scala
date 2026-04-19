/*
 * Port of scala-js javalib Double, adapted for the ScalaPy backend.
 */

package java.lang

import java.lang.constant.{Constable, ConstantDesc}

import scala.python.runtime.{PyBuiltins, PyStruct}

final class Double private ()
    extends Number with Comparable[Double] with Constable with ConstantDesc {

  def this(value: scala.Double) = this()
  def this(s: String) = this()

  @inline def doubleValue(): scala.Double =
    this.asInstanceOf[scala.Double]

  @inline override def byteValue(): scala.Byte = doubleValue().toByte
  @inline override def shortValue(): scala.Short = doubleValue().toShort
  @inline def intValue(): scala.Int = doubleValue().toInt
  @inline def longValue(): scala.Long = doubleValue().toLong
  @inline def floatValue(): scala.Float = doubleValue().toFloat

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    Double.hashCode(doubleValue())

  @inline override def compareTo(that: Double): Int =
    Double.compare(doubleValue(), that.doubleValue())

  @inline override def toString(): String =
    Double.toString(doubleValue())

  @inline def isNaN(): scala.Boolean =
    Double.isNaN(doubleValue())

  @inline def isInfinite(): scala.Boolean =
    Double.isInfinite(doubleValue())
}

object Double {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Double]

  final val POSITIVE_INFINITY = 1.0 / 0.0
  final val NEGATIVE_INFINITY = 1.0 / -0.0
  final val NaN = 0.0 / 0.0
  final val MAX_VALUE = scala.Double.MaxValue
  final val MIN_NORMAL = 2.2250738585072014e-308
  final val MIN_VALUE = scala.Double.MinPositiveValue
  final val MAX_EXPONENT = 1023
  final val MIN_EXPONENT = -1022
  final val SIZE = 64
  final val BYTES = 8

  private final val PosInfinityBits = 0x7ff0000000000000L
  private final val CanonicalNaNBits = 0x7ff8000000000000L

  @inline def `new`(value: scala.Double): Double = valueOf(value)
  @inline def `new`(s: String): Double = valueOf(s)

  @inline def valueOf(d: scala.Double): Double = d.asInstanceOf[Double]
  @inline def valueOf(s: String): Double = valueOf(parseDouble(s))

  def parseDouble(s: String): scala.Double = {
    val normalized = normalizeLiteralOrFail(s)
    try
      if (isHexLiteral(normalized)) PyBuiltins.float_from_hex(normalized)
      else PyBuiltins.float_parse(normalized)
    catch
      case _: Throwable => parseFail(s)
  }

  private def normalizeLiteralOrFail(s: String): String = {
    if (s == null) parseFail(s)
    val trimmed = s.trim()
    if (trimmed.isEmpty) parseFail(s)
    val trimmedLength = trimmed.length()
    val last = trimmed.charAt(trimmedLength - 1)
    val normalized =
      if (last == 'f' || last == 'F' || last == 'd' || last == 'D')
        trimmed.substring(0, trimmedLength - 1)
      else
        trimmed
    if (normalized.isEmpty || normalized == "+" || normalized == "-")
      parseFail(s)
    normalized
  }

  private def isHexLiteral(text: String): scala.Boolean = {
    val unsigned =
      if (text.startsWith("+") || text.startsWith("-")) text.substring(1)
      else text
    (unsigned.startsWith("0x") || unsigned.startsWith("0X")) &&
      (unsigned.indexOf("p") >= 0 || unsigned.indexOf("P") >= 0)
  }

  private def parseFail(s: String): Nothing =
    throw new NumberFormatException(s"""For input string: "$s"""")

  def toString(d: scala.Double): String =
    if (isNaN(d)) "NaN"
    else if (d == POSITIVE_INFINITY) "Infinity"
    else if (d == NEGATIVE_INFINITY) "-Infinity"
    else "" + d

  def toHexString(d: scala.Double): String =
    FloatDouble.toHexString(d)

  @inline def hashCode(value: scala.Double): Int =
    Long.hashCode(doubleToLongBits(value))

  @inline def compare(x: scala.Double, y: scala.Double): scala.Int = {
    if (x < y) -1
    else if (x > y) 1
    else {
      val xBits = doubleToLongBits(x)
      val yBits = doubleToLongBits(y)
      if (xBits == yBits) 0
      else if (xBits < yBits) -1
      else 1
    }
  }

  @inline def sum(a: scala.Double, b: scala.Double): scala.Double = a + b
  @inline def max(a: scala.Double, b: scala.Double): scala.Double = Math.max(a, b)
  @inline def min(a: scala.Double, b: scala.Double): scala.Double = Math.min(a, b)

  @inline def isNaN(value: scala.Double): scala.Boolean = value != value
  @inline def isInfinite(value: scala.Double): scala.Boolean =
    value == POSITIVE_INFINITY || value == NEGATIVE_INFINITY
  @inline def isFinite(value: scala.Double): scala.Boolean =
    !isNaN(value) && !isInfinite(value)

  @inline def doubleToRawLongBits(value: scala.Double): scala.Long =
    PyStruct.double_to_int64_bits(value)

  @inline def doubleToLongBits(value: scala.Double): scala.Long = {
    val rawBits = doubleToRawLongBits(value)
    if (isSpecialBitPattern(rawBits) && (rawBits & 0x000fffffffffffffL) != 0L) CanonicalNaNBits
    else rawBits
  }

  @inline def longBitsToDouble(bits: scala.Long): scala.Double =
    PyStruct.double_from_int64_bits(bits)

  @inline private[lang] def isSpecialBitPattern(bits: scala.Long): scala.Boolean =
    (bits & PosInfinityBits) == PosInfinityBits
}
