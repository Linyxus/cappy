/*
 * Port of scala-js javalib Byte, adapted for the ScalaPy backend.
 */

package java.lang

import java.lang.constant.Constable

final class Byte private () extends Number with Comparable[Byte] with Constable {

  def this(value: scala.Byte) = this()
  def this(s: String) = this()

  @inline override def byteValue(): scala.Byte =
    this.asInstanceOf[scala.Byte]

  @inline override def shortValue(): scala.Short = byteValue().toShort
  @inline def intValue(): scala.Int = byteValue().toInt
  @inline def longValue(): scala.Long = byteValue().toLong
  @inline def floatValue(): scala.Float = byteValue().toFloat
  @inline def doubleValue(): scala.Double = byteValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    byteValue().toInt

  @inline override def compareTo(that: Byte): Int =
    Byte.compare(byteValue(), that.byteValue())

  @inline override def toString(): String =
    Byte.toString(byteValue())
}

object Byte {
  val TYPE: Class[?] = scala.Predef.classOf[scala.Byte]

  final val SIZE = 8
  final val BYTES = 1

  def MIN_VALUE: scala.Byte = -128
  def MAX_VALUE: scala.Byte = 127

  @inline def `new`(value: scala.Byte): Byte = valueOf(value)

  @inline def `new`(s: String): Byte = valueOf(s)

  @inline def valueOf(b: scala.Byte): Byte = b.asInstanceOf[Byte]

  @inline def valueOf(s: String): Byte = valueOf(parseByte(s))

  @inline def valueOf(s: String, radix: Int): Byte =
    valueOf(parseByte(s, radix))

  @inline def parseByte(s: String): scala.Byte = parseByte(s, 10)

  def parseByte(s: String, radix: Int): scala.Byte = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      r.toByte
  }

  @inline def toString(b: scala.Byte): String =
    "" + b

  @noinline def decode(nm: String): Byte =
    Integer.decodeGeneric(nm, valueOf(_, _))

  @inline def compare(x: scala.Byte, y: scala.Byte): scala.Int =
    x - y

  @inline def toUnsignedInt(x: scala.Byte): scala.Int =
    x.toInt & 0xff

  @inline def toUnsignedLong(x: scala.Byte): scala.Long =
    toUnsignedInt(x).toLong
}
