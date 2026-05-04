package scala.runtime

import java.io.Serializable

final class ByteRef(var elem: Byte) extends Serializable:
  override def toString: String = java.lang.Byte.toString(elem)

object ByteRef:
  def create(e: Byte): ByteRef = new ByteRef(e)
  def zero(): ByteRef = new ByteRef(0.toByte)
