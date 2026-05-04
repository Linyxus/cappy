package scala.runtime

import java.io.Serializable

final class VolatileShortRef(var elem: Short) extends Serializable:
  override def toString: String = java.lang.Short.toString(elem)

object VolatileShortRef:
  def create(e: Short): VolatileShortRef = new VolatileShortRef(e)
  def zero(): VolatileShortRef = new VolatileShortRef(0.toShort)
