package scala.runtime

import java.io.Serializable

final class BooleanRef(var elem: Boolean) extends Serializable:
  override def toString: String = String.valueOf(elem)

object BooleanRef:
  def create(e: Boolean): BooleanRef = new BooleanRef(e)
  def zero(): BooleanRef = new BooleanRef(false)
