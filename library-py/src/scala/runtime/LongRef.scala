package scala.runtime

import java.io.Serializable

final class LongRef(var elem: Long) extends Serializable:
  override def toString: String = java.lang.Long.toString(elem)

object LongRef:
  def create(e: Long): LongRef = new LongRef(e)
  def zero(): LongRef = new LongRef(0L)
