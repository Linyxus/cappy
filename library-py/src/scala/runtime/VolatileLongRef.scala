package scala.runtime

import java.io.Serializable

final class VolatileLongRef(var elem: Long) extends Serializable:
  override def toString: String = java.lang.Long.toString(elem)

object VolatileLongRef:
  def create(e: Long): VolatileLongRef = new VolatileLongRef(e)
  def zero(): VolatileLongRef = new VolatileLongRef(0L)
