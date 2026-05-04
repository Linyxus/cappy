package scala.runtime

import java.io.Serializable

final class VolatileByteRef(var elem: Byte) extends Serializable:
  override def toString: String = java.lang.Byte.toString(elem)

object VolatileByteRef:
  def create(e: Byte): VolatileByteRef = new VolatileByteRef(e)
  def zero(): VolatileByteRef = new VolatileByteRef(0.toByte)
