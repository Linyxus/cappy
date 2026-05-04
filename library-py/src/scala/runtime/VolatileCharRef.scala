package scala.runtime

import java.io.Serializable

final class VolatileCharRef(var elem: Char) extends Serializable:
  override def toString: String = java.lang.Character.toString(elem)

object VolatileCharRef:
  def create(e: Char): VolatileCharRef = new VolatileCharRef(e)
  def zero(): VolatileCharRef = new VolatileCharRef(0.toChar)
