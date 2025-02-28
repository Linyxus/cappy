package cappy.core
import cappy.io.SourceFile

case class Span(start: Int, end: Int)

case class SourcePos(file: SourceFile, span: Span)

trait Positioned:
  private var _pos: SourcePos | Null = null

  def hasPos: Boolean = _pos != null
  def pos: SourcePos = _pos.nn
  def setPos(pos: SourcePos): Unit =
    _pos = pos
  def withPos(pos: SourcePos): this.type =
    setPos(pos)
    this
