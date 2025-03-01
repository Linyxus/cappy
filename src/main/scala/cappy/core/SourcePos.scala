package cappy.core
import cappy.io.SourceFile

case class Span(start: Int, end: Int):
  def merge(other: Span): Span =
    Span(Math.min(start, other.start), Math.max(end, other.end))

case class SourcePos(file: SourceFile, span: Span):
  def merge(other: SourcePos): SourcePos =
    assert(file == other.file, s"cannot merge spans from different files: $file and ${other.file}")
    SourcePos(file, span.merge(other.span))

trait Positioned:
  private var _pos: SourcePos | Null = null

  def hasPos: Boolean = _pos != null
  def pos: SourcePos = _pos.nn
  def setPos(pos: SourcePos): Unit =
    _pos = pos
  def withPos(pos: SourcePos): this.type =
    setPos(pos)
    this
