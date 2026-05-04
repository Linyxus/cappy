package scala.runtime

import java.io.Serializable

final class ShortRef(var elem: Short) extends Serializable:
  override def toString: String = java.lang.Short.toString(elem)

object ShortRef:
  def create(e: Short): ShortRef = new ShortRef(e)
  def zero(): ShortRef = new ShortRef(0.toShort)
