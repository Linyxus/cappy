package scala.runtime

import java.io.Serializable

final class VolatileFloatRef(var elem: Float) extends Serializable:
  override def toString: String = java.lang.Float.toString(elem)

object VolatileFloatRef:
  def create(e: Float): VolatileFloatRef = new VolatileFloatRef(e)
  def zero(): VolatileFloatRef = new VolatileFloatRef(0.0f)
