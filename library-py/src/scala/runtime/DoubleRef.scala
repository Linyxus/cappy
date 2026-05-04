package scala.runtime

import java.io.Serializable

final class DoubleRef(var elem: Double) extends Serializable:
  override def toString: String = java.lang.Double.toString(elem)

object DoubleRef:
  def create(e: Double): DoubleRef = new DoubleRef(e)
  def zero(): DoubleRef = new DoubleRef(0.0)
