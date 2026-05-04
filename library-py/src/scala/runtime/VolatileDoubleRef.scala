package scala.runtime

import java.io.Serializable

final class VolatileDoubleRef(var elem: Double) extends Serializable:
  override def toString: String = java.lang.Double.toString(elem)

object VolatileDoubleRef:
  def create(e: Double): VolatileDoubleRef = new VolatileDoubleRef(e)
  def zero(): VolatileDoubleRef = new VolatileDoubleRef(0.0)
