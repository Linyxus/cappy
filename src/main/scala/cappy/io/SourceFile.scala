package cappy.io

import java.nio.file.{Files, Path}
import scala.io.Source

case class SourceFile(name: String, content: String)

object SourceFile:
  def fromPath(path: Path): SourceFile =
    val content = Files.readString(path)
    SourceFile(path.getFileName.toString, content)

  def fromPath(path: String): SourceFile =
    fromPath(Path.of(path))
