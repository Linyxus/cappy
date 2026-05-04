package scala.runtime

import java.io.Serializable

final class CharRef(var elem: Char) extends Serializable:
  override def toString: String = java.lang.Character.toString(elem)

object CharRef:
  def create(e: Char): CharRef = new CharRef(e)
  def zero(): CharRef = new CharRef(0.toChar)
