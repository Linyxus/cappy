package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.serialization.{PyIRDeserializer, PyIRException}
import dotty.tools.dotc.core.Contexts.Context

import java.io.File
import java.util.jar.JarFile
import scala.collection.mutable

/** Walks the compile classpath AND the compile's output directory for
 *  `.pyir` entries and deserializes them into `PyLinker.Input` values.
 *
 *  Mirrors Scala.js's `PathIRContainer.fromClasspath` + the `scalaJSIR`
 *  sbt task, which reads `.sjsir` via the `fullClasspath` (which always
 *  includes the compile output). See
 *  `inbox/scala-js/linker/jvm/src/main/scala/org/scalajs/linker/PathIRContainer.scala`.
 *
 *  In Scala.js the linker is a separate sbt task run after compile, so
 *  the output directory already contains fresh `.sjsir` files by the
 *  time the linker walks it. The Python backend currently folds the
 *  linker into `GenPython.linkAndWrite`, so the caller is responsible
 *  for writing this CU's `.pyir` before invoking `loadInputs`.
 *
 *  Results are deduped by canonical file path so that `.pyir` reachable
 *  both via the output directory and via an overlapping classpath entry
 *  (e.g. default `-cp .` + default `-d .`) is only loaded once.
 */
object PyClasspathLoader:

  def loadInputs(using ctx: Context): List[PyLinker.Input] =
    val cp = ctx.settings.classpath.value
    val outDir = outputDirCanonical
    val inputs = mutable.ListBuffer.empty[PyLinker.Input]
    val visitedFiles = mutable.HashSet.empty[String]

    // Scan the output directory first so its fresh `.pyir` wins over
    // any overlapping classpath entry.
    outDir.foreach { dir =>
      val f = new File(dir)
      if f.isDirectory then loadFromDir(f, visitedFiles, inputs)
    }

    for entry <- cp.split(File.pathSeparator) do
      val f = new File(entry)
      if f.isFile && f.getName.endsWith(".jar") then
        loadFromJar(f, visitedFiles, inputs)
      else if f.isDirectory then
        loadFromDir(f, visitedFiles, inputs)

    inputs.toList

  private def outputDirCanonical(using ctx: Context): Option[String] =
    val out = ctx.settings.outputDir.value
    val jpath = out.jpath
    if jpath == null then None
    else try Some(jpath.toFile.getCanonicalPath.nn) catch case _: java.io.IOException => None

  private def loadFromJar(
      jarFile: File,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    val canon =
      try jarFile.getCanonicalPath.nn catch case _: java.io.IOException => jarFile.getAbsolutePath.nn
    if !visitedFiles.add(canon) then return
    try
      val jar = new JarFile(jarFile)
      try
        val entries = jar.entries()
        while entries.hasMoreElements do
          val entry = entries.nextElement()
          if !entry.isDirectory && entry.getName.nn.endsWith(".pyir") then
            val is = jar.getInputStream(entry)
            try
              val bytes = is.readAllBytes()
              val cu = PyIRDeserializer.deserialize(bytes)
              if cu.classes.nonEmpty then
                inputs += PyLinker.Input(cu.classes, cu.mainEntry)
            finally is.close()
      finally jar.close()
    catch
      case e: PyIRException =>
        System.err.println(s"[scalapy] warning: skipping ${jarFile.getName}: ${e.getMessage}")
      case e: java.io.IOException =>
        System.err.println(s"[scalapy] warning: cannot read ${jarFile.getName}: ${e.getMessage}")

  private def loadFromDir(
      dir: File,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    def walk(d: File): Unit =
      val children = d.listFiles()
      if children != null then
        for child <- children do
          if child.isDirectory then walk(child)
          else if child.getName.endsWith(".pyir") then
            val canon =
              try child.getCanonicalPath.nn catch case _: java.io.IOException => child.getAbsolutePath.nn
            if visitedFiles.add(canon) then
              try
                val bytes = java.nio.file.Files.readAllBytes(child.toPath)
                val cu = PyIRDeserializer.deserialize(bytes)
                if cu.classes.nonEmpty then
                  inputs += PyLinker.Input(cu.classes, cu.mainEntry)
              catch
                case e: PyIRException =>
                  System.err.println(s"[scalapy] warning: skipping ${child.getName}: ${e.getMessage}")
                case e: java.io.IOException =>
                  System.err.println(s"[scalapy] warning: cannot read ${child.getName}: ${e.getMessage}")
    walk(dir)
