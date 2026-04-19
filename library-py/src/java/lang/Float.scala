/*
 * Port of scala-js javalib Float, adapted for the ScalaPy backend.
 */

package java.lang

import java.lang.constant.{Constable, ConstantDesc}

import scala.python.runtime.{PyBuiltins, PyStruct}

final class Float private ()
    extends Number with Comparable[Float] with Constable with ConstantDesc {

  def this(value: scala.Float) = this()
  def this(s: String) = this()

  @inline def floatValue(): scala.Float =
    this.asInstanceOf[scala.Float]

  @inline override def byteValue(): scala.Byte = floatValue().toByte
  @inline override def shortValue(): scala.Short = floatValue().toShort
  @inline def intValue(): scala.Int = floatValue().toInt
  @inline def longValue(): scala.Long = floatValue().toLong
  @inline def doubleValue(): scala.Double = floatValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    Float.hashCode(floatValue())

  @inline override def compareTo(that: Float): Int =
    Float.compare(floatValue(), that.floatValue())

  @inline override def toString(): String =
    Float.toString(floatValue())

  @inline def isNaN(): scala.Boolean =
    Float.isNaN(floatValue())

  @inline def isInfinite(): scala.Boolean =
    Float.isInfinite(floatValue())
}

object Float {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Float]

  final val POSITIVE_INFINITY = 1.0f / 0.0f
  final val NEGATIVE_INFINITY = 1.0f / -0.0f
  final val NaN = 0.0f / 0.0f
  final val MAX_VALUE = scala.Float.MaxValue
  final val MIN_NORMAL = 1.17549435e-38f
  final val MIN_VALUE = scala.Float.MinPositiveValue
  final val MAX_EXPONENT = 127
  final val MIN_EXPONENT = -126
  final val SIZE = 32
  final val BYTES = 4

  private final val PosInfinityBits = 0x7f800000
  private final val CanonicalNaNBits = 0x7fc00000

  @inline def `new`(value: scala.Float): Float = valueOf(value)
  @inline def `new`(value: scala.Double): Float = valueOf(value.toFloat)
  @inline def `new`(s: String): Float = valueOf(s)

  @inline def valueOf(f: scala.Float): Float = f.asInstanceOf[Float]
  @inline def valueOf(s: String): Float = valueOf(parseFloat(s))

  def parseFloat(s: String): scala.Float = {
    val normalized = normalizeLiteralOrFail(s)
    try
      val parsed =
        if (isHexLiteral(normalized)) PyBuiltins.float_from_hex(normalized)
        else PyBuiltins.float_parse(normalized)
      parsed.toFloat
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

  def toString(f: scala.Float): String =
    if (isNaN(f)) "NaN"
    else if (f == POSITIVE_INFINITY) "Infinity"
    else if (f == NEGATIVE_INFINITY) "-Infinity"
    else "" + f

  def toHexString(f: scala.Float): String =
    FloatDouble.toHexString(f)

  @inline def hashCode(value: scala.Float): Int =
    floatToIntBits(value)

  @inline def compare(x: scala.Float, y: scala.Float): scala.Int = {
    if (x < y) -1
    else if (x > y) 1
    else {
      val xBits = floatToIntBits(x)
      val yBits = floatToIntBits(y)
      if (xBits == yBits) 0
      else if (xBits < yBits) -1
      else 1
    }
  }

  @inline def sum(a: scala.Float, b: scala.Float): scala.Float = a + b
  @inline def max(a: scala.Float, b: scala.Float): scala.Float = Math.max(a, b)
  @inline def min(a: scala.Float, b: scala.Float): scala.Float = Math.min(a, b)

  @inline def isNaN(value: scala.Float): scala.Boolean = value != value
  @inline def isInfinite(value: scala.Float): scala.Boolean =
    value == POSITIVE_INFINITY || value == NEGATIVE_INFINITY
  @inline def isFinite(value: scala.Float): scala.Boolean =
    !isNaN(value) && !isInfinite(value)

  @inline def floatToRawIntBits(value: scala.Float): scala.Int =
    PyStruct.float_to_int32_bits(value)

  @inline def floatToIntBits(value: scala.Float): scala.Int = {
    val rawBits = floatToRawIntBits(value)
    if (isSpecialBitPattern(rawBits) && (rawBits & 0x007fffff) != 0) CanonicalNaNBits
    else rawBits
  }

  @inline def intBitsToFloat(bits: scala.Int): scala.Float =
    PyStruct.float_from_int32_bits(bits)

  @inline private[lang] def isSpecialBitPattern(bits: scala.Int): scala.Boolean =
    (bits & PosInfinityBits) == PosInfinityBits
}
