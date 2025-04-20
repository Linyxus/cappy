package cavia
import io.*
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

case class Testcase(path: Path, source: SourceFile, checkFile: Option[String])

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

/** Load all testcases in the given directory, along with their expected output. */
def loadAllTestcases(dir: String): List[Testcase] =
  val path = Paths.get(dir)
  if !Files.exists(path) then
    return List.empty

  def retrieveCheckFile(path: Path): Option[String] =
    val checkPath = path.resolveSibling(path.getFileName.toString.replace(".scala", ".check"))
    if Files.exists(checkPath) then
      Some(SourceFile.fromPath(checkPath).content)
    else
      None

  Files.walk(path)
    .iterator()
    .asScala
    .filter(p => !Files.isDirectory(p) && p.toString.endsWith(".scala"))
    .map(p => Testcase(p, SourceFile.fromPath(p), retrieveCheckFile(p)))
    .toList
