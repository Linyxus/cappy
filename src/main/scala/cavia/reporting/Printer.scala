package cavia
package reporting

import core.*
import io.SourceFile
import parsing.Parser.*

object Printer:
  def showSourcePos(pos: SourcePos, message: List[String]): String =
    val lines = pos.file.content.linesWithSeparators.toList
    def locate(pos: Int): (Int, Int) =
      var lineIdx = 0
      var colIdx = 0
      var rem = pos
      while lineIdx < lines.length && rem > lines(lineIdx).length do
        rem -= lines(lineIdx).length
        lineIdx += 1
      colIdx = rem
      (lineIdx, colIdx)
    
    val sb = StringBuilder()

    val (startLine, startCol) = locate(pos.span.start)
    val (endLine, endCol) = locate(pos.span.end)

    // CREDIT NOTE: the following code is authored by `claude-3.7-sonnet-thinking`, big thanks to them
    // Line number width for formatting
    val maxLineNumWidth = (endLine + 1).toString.length
    
    // Function to format a line number
    def formatLineNum(lineNum: Int): String =
      val lineNumStr = (lineNum + 1).toString
      " " * (maxLineNumWidth - lineNumStr.length) + lineNumStr + " | "

    // Display affected lines with line numbers
    for lineIdx <- startLine to endLine do
      // Add line number and separator
      sb.append(formatLineNum(lineIdx))
      
      // Add the actual source line content (trim the line separator)
      val lineContent = lines(lineIdx).stripLineEnd
      sb.append(lineContent)
      sb.append("\n")
      
      // Add the highlight line with ^ characters
      sb.append(" " * maxLineNumWidth + " | ")

      if lineIdx == startLine && lineIdx == endLine then
        // Single-line span
        sb.append(" " * startCol)
        sb.append("^" * (if endCol - startCol > 0 then endCol - startCol else 1))
      else if lineIdx == startLine then
        // First line of multi-line span
        sb.append(" " * startCol)
        sb.append("^" * (lineContent.length - startCol))
      else if lineIdx == endLine then
        // Last line of multi-line span
        sb.append("^" * endCol)
      else
        // Middle line of multi-line span
        sb.append("^" * lineContent.length)

      sb.append("\n")

    // Add messages
    if message.nonEmpty then
      sb.append("\n")
      message.foreach { msg =>
        sb.append(" " * maxLineNumWidth + "   " + msg + "\n")
      }

    sb.result()

  def getErrorMessages(err: ParseError): List[String] =
    def collectDescs(err: ParseError): List[String] = err match
      case ParseError.Here(msg) =>
        if msg == null then
          List("ERROR(parsing): unknown error")
        else
          List("ERROR(parsing): " + msg)
      case ParseError.When(inner, desc) => ("when parsing " + desc) :: collectDescs(inner)
    collectDescs(err).reverse

  def showParseError(error: ParseError): String =
    val msgs = getErrorMessages(error)
    showSourcePos(error.pos, msgs)

  extension (err: ParseError)
    def show: String = showParseError(err)
