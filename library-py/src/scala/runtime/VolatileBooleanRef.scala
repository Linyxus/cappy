package scala.runtime

import java.io.Serializable

final class VolatileBooleanRef(var elem: Boolean) extends Serializable:
  override def toString: String = String.valueOf(elem)

object VolatileBooleanRef:
  def create(e: Boolean): VolatileBooleanRef = new VolatileBooleanRef(e)
  def zero(): VolatileBooleanRef = new VolatileBooleanRef(false)
