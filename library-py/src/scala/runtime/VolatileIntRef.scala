package scala.runtime

import java.io.Serializable

final class VolatileIntRef(var elem: Int) extends Serializable:
  override def toString: String = java.lang.Integer.toString(elem)

object VolatileIntRef:
  def create(e: Int): VolatileIntRef = new VolatileIntRef(e)
  def zero(): VolatileIntRef = new VolatileIntRef(0)
