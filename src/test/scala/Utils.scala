package cappy
import io.*
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

def loadAllSourceFiles(dir: String): List[SourceFile] =
  val path = Paths.get(dir)
  if !Files.exists(path) then
    return List.empty
  
  Files.walk(path)
    .iterator()
    .asScala
    .filter(p => !Files.isDirectory(p) && p.toString.endsWith(".scala"))
    .map(p => SourceFile.fromPath(p))
    .toList
