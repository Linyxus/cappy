package cappy
package reporting

import core.*
import io.SourceFile

object Printer:
  def showSourcePos(pos: SourcePos, message: List[String]): String =
    val lines = pos.file.content.linesWithSeparators.toList
    def locate: (Int, Int) =
      var lineIdx = 0
      ???
    ???
