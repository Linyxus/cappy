/*
 * Port of scala-js javalib Void, adapted for the ScalaPy backend.
 */

package java.lang

final class Void private () extends AnyRef {
  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int = 0

  @inline override def toString(): String = "()"
}

object Void {
  val TYPE: Class[?] = scala.Predef.classOf[scala.Unit]
}
