package scala.runtime

import java.io.Serializable

final class VolatileObjectRef[T](var elem: T) extends Serializable:
  override def toString: String = String.valueOf(elem)

object VolatileObjectRef:
  def create[U](e: U): VolatileObjectRef[U] = new VolatileObjectRef(e)
  def zero(): VolatileObjectRef[AnyRef] = new VolatileObjectRef[AnyRef](null.asInstanceOf[AnyRef])
