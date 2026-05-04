package scala.runtime

import java.io.Serializable

final class FloatRef(var elem: Float) extends Serializable:
  override def toString: String = java.lang.Float.toString(elem)

object FloatRef:
  def create(e: Float): FloatRef = new FloatRef(e)
  def zero(): FloatRef = new FloatRef(0.0f)
