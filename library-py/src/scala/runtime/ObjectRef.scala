package scala.runtime

import java.io.Serializable

final class ObjectRef[T](var elem: T) extends Serializable:
  override def toString: String = String.valueOf(elem)

object ObjectRef:
  def create[U](e: U): ObjectRef[U] = new ObjectRef(e)
  def zero(): ObjectRef[AnyRef] = new ObjectRef[AnyRef](null.asInstanceOf[AnyRef])
