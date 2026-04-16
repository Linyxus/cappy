package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.serialization.{PyIRDeserializer, PyIRException}
import dotty.tools.dotc.core.Contexts.Context

import java.io.File
import java.util.jar.JarFile
import scala.collection.mutable

/** Walks the compile classpath for `.pyir` entries and deserializes them
 *  into `PyLinker.Input` values that can be prepended to a user CU's
 *  link inputs.
 *
 *  Results are cached by classpath string: within a single `Run` (or any
 *  sequence of compilations sharing the same classpath) the walk happens
 *  only once.
 */
object PyClasspathLoader:

  @volatile private var cache: (String, List[PyLinker.Input]) | Null = null

  def loadInputs(using ctx: Context): List[PyLinker.Input] =
    val cp = ctx.settings.classpath.value
    cache match
      case (cachedCp, inputs) if cachedCp == cp => inputs
      case _ =>
        val inputs = doLoad(cp)
        cache = (cp, inputs)
        inputs

  private def doLoad(classpath: String): List[PyLinker.Input] =
    val inputs = mutable.ListBuffer.empty[PyLinker.Input]
    val entries = classpath.split(File.pathSeparator)
    for entry <- entries do
      val f = new File(entry)
      if f.isFile && f.getName.endsWith(".jar") then
        loadFromJar(f, inputs)
      else if f.isDirectory then
        loadFromDir(f, inputs)
    inputs.toList

  private def loadFromJar(
      jarFile: File,
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
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
        // Log and skip corrupt/version-mismatched files
        System.err.println(s"[scalapy] warning: skipping ${jarFile.getName}: ${e.getMessage}")
      case e: java.io.IOException =>
        System.err.println(s"[scalapy] warning: cannot read ${jarFile.getName}: ${e.getMessage}")

  private def loadFromDir(
      dir: File,
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    val pyirFiles = collectPyIR(dir)
    for f <- pyirFiles do
      try
        val bytes = java.nio.file.Files.readAllBytes(f.toPath)
        val cu = PyIRDeserializer.deserialize(bytes)
        if cu.classes.nonEmpty then
          inputs += PyLinker.Input(cu.classes, cu.mainEntry)
      catch
        case e: PyIRException =>
          System.err.println(s"[scalapy] warning: skipping ${f.getName}: ${e.getMessage}")
        case e: java.io.IOException =>
          System.err.println(s"[scalapy] warning: cannot read ${f.getName}: ${e.getMessage}")

  private def collectPyIR(dir: File): List[File] =
    val buf = mutable.ListBuffer.empty[File]
    def walk(d: File): Unit =
      val children = d.listFiles()
      if children != null then
        for child <- children do
          if child.isDirectory then walk(child)
          else if child.getName.endsWith(".pyir") then buf += child
    walk(dir)
    buf.toList
