/*
 * Port of scala-js javalib Short, adapted for the ScalaPy backend.
 */

package java.lang

import java.lang.constant.Constable

final class Short private () extends Number with Comparable[Short] with Constable {

  def this(value: scala.Short) = this()
  def this(s: String) = this()

  @inline override def shortValue(): scala.Short =
    this.asInstanceOf[scala.Short]

  @inline override def byteValue(): scala.Byte = shortValue().toByte
  @inline def intValue(): scala.Int = shortValue().toInt
  @inline def longValue(): scala.Long = shortValue().toLong
  @inline def floatValue(): scala.Float = shortValue().toFloat
  @inline def doubleValue(): scala.Double = shortValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    shortValue().toInt

  @inline override def compareTo(that: Short): Int =
    Short.compare(shortValue(), that.shortValue())

  @inline override def toString(): String =
    Short.toString(shortValue())

}

object Short {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Short]

  final val SIZE = 16
  final val BYTES = 2

  final val MIN_VALUE: scala.Short = -32768
  final val MAX_VALUE: scala.Short = 32767

  @inline def `new`(value: scala.Short): Short = valueOf(value)

  @inline def `new`(s: String): Short = valueOf(s)

  @inline def valueOf(s: scala.Short): Short = s.asInstanceOf[Short]

  @inline def valueOf(s: String): Short = valueOf(parseShort(s))

  @inline def valueOf(s: String, radix: Int): Short =
    valueOf(parseShort(s, radix))

  @inline def parseShort(s: String): scala.Short = parseShort(s, 10)

  def parseShort(s: String, radix: Int): scala.Short = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE.toInt || r > MAX_VALUE.toInt)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      narrowToShort(r)
  }

  @inline def toString(s: scala.Short): String =
    "" + s.toInt

  @noinline def decode(nm: String): Short =
    Integer.decodeGeneric(nm, valueOf(_, _))

  @inline def compare(x: scala.Short, y: scala.Short): scala.Int =
    x.toInt - y.toInt

  def reverseBytes(i: scala.Short): scala.Short =
    narrowToShortSigned(((i.toInt >>> 8) & 0xff) | ((i.toInt & 0xff) << 8))

  @inline def toUnsignedInt(x: scala.Short): scala.Int =
    x.toInt & 0xffff

  @inline def toUnsignedLong(x: scala.Short): scala.Long =
    toUnsignedInt(x).toLong

  @inline private def narrowToShort(value: Int): scala.Short =
    value.asInstanceOf[scala.Short]

  @inline private def narrowToShortSigned(value: Int): scala.Short =
    narrowToShort(if value >= 0x8000 then value - 0x10000 else value)
}
