package cavia
package reporting
import core.*

case class MessagePart(lines: List[String], pos: SourcePos)

case class Message(parts: List[MessagePart]):
  def show: String =
    val partsStr = parts.map: part =>
      if part.pos ne null then
        Printer.showSourcePos(part.pos, part.lines)
      else part.lines.mkString("\n")
    partsStr.mkString("\n")

object Message:
  def simple(msg: String, pos: SourcePos): Message =
    Message(List(MessagePart(List(msg), pos)))

  def multiline(msgs: List[String], pos: SourcePos): Message =
    Message(List(MessagePart(msgs, pos)))

case class EntityWithProvenance(entity: String, provenancePos: SourcePos, provenance: String)
