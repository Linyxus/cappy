/*
 * Port of scala-js javalib Boolean, adapted for the ScalaPy backend.
 */

package java.lang

import java.lang.constant.Constable

final class Boolean private ()
    extends AnyRef with java.io.Serializable with Comparable[Boolean] with Constable {

  def this(value: scala.Boolean) = this()
  def this(v: String) = this()

  @inline def booleanValue(): scala.Boolean =
    this.asInstanceOf[scala.Boolean]

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    if (booleanValue()) 1231 else 1237

  @inline override def compareTo(that: Boolean): Int =
    Boolean.compare(booleanValue(), that.booleanValue())

  @inline override def toString(): String =
    Boolean.toString(booleanValue())

}

object Boolean {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Boolean]

  @inline def TRUE: Boolean = valueOf(true)
  @inline def FALSE: Boolean = valueOf(false)

  @inline def `new`(value: scala.Boolean): Boolean = valueOf(value)

  @inline def `new`(s: String): Boolean = valueOf(s)

  @inline def valueOf(b: scala.Boolean): Boolean = b.asInstanceOf[Boolean]

  @inline def valueOf(s: String): Boolean = valueOf(parseBoolean(s))

  @inline def parseBoolean(s: String): scala.Boolean =
    (s != null) && s.equalsIgnoreCase("true")

  @inline def toString(b: scala.Boolean): String =
    "" + b

  @inline def compare(x: scala.Boolean, y: scala.Boolean): scala.Int =
    if (x == y) 0 else if (x) 1 else -1
}
