package scala.runtime

import java.io.Serializable

final class IntRef(var elem: Int) extends Serializable:
  override def toString: String = java.lang.Integer.toString(elem)

object IntRef:
  def create(e: Int): IntRef = new IntRef(e)
  def zero(): IntRef = new IntRef(0)
