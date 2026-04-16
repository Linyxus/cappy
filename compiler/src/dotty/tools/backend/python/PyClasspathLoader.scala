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
    // any overlapping classpath entry. Output-directory entries are
    // tagged `User` (the CU's own freshly-emitted classes, always kept
    // by the linker), classpath entries are tagged `Support` (DCE
    // candidates).
    //
    // The output-dir scan is NON-RECURSIVE: emission writes
    // `<sourceName>.pyir` directly at the top of the output dir, so
    // subtrees (e.g. `library-py/target/.../classes/*.pyir` when
    // `-d .` happens to equal cwd) are not our output. They will be
    // picked up by the classpath scan below (as `Support`), so nothing
    // is lost.
    outDir.foreach { dir =>
      val f = new File(dir)
      if f.isDirectory then
        loadFromDirTopLevel(f, PyLinker.InputSource.User, visitedFiles, inputs)
    }

    for entry <- cp.split(File.pathSeparator) do
      val f = new File(entry)
      if f.isFile && f.getName.endsWith(".jar") then
        loadFromJar(f, PyLinker.InputSource.Support, visitedFiles, inputs)
      else if f.isDirectory then
        loadFromDir(f, PyLinker.InputSource.Support, visitedFiles, inputs)

    inputs.toList

  private def outputDirCanonical(using ctx: Context): Option[String] =
    val out = ctx.settings.outputDir.value
    val jpath = out.jpath
    if jpath == null then None
    else try Some(jpath.toFile.getCanonicalPath.nn) catch case _: java.io.IOException => None

  private def loadFromJar(
      jarFile: File,
      source: PyLinker.InputSource,
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
                inputs += PyLinker.Input(cu.classes, cu.mainEntry, source)
            finally is.close()
      finally jar.close()
    catch
      case e: PyIRException =>
        System.err.println(s"[scalapy] warning: skipping ${jarFile.getName}: ${e.getMessage}")
      case e: java.io.IOException =>
        System.err.println(s"[scalapy] warning: cannot read ${jarFile.getName}: ${e.getMessage}")

  private def loadFromDir(
      dir: File,
      source: PyLinker.InputSource,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    def walk(d: File): Unit =
      val children = d.listFiles()
      if children != null then
        for child <- children do
          if child.isDirectory then walk(child)
          else if child.getName.endsWith(".pyir") then
            ingest(child, source, visitedFiles, inputs)
    walk(dir)

  /** Scan only the direct contents of `dir`, not subdirectories.
   *  Used for the output-directory scan: emitted `.pyir` files live
   *  flat at the top of the output dir, and recursing would pick up
   *  unrelated trees when the output dir overlaps the classpath. */
  private def loadFromDirTopLevel(
      dir: File,
      source: PyLinker.InputSource,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    val children = dir.listFiles()
    if children != null then
      for child <- children do
        if !child.isDirectory && child.getName.endsWith(".pyir") then
          ingest(child, source, visitedFiles, inputs)

  private def ingest(
      child: File,
      source: PyLinker.InputSource,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  ): Unit =
    val canon =
      try child.getCanonicalPath.nn catch case _: java.io.IOException => child.getAbsolutePath.nn
    if visitedFiles.add(canon) then
      try
        val bytes = java.nio.file.Files.readAllBytes(child.toPath)
        val cu = PyIRDeserializer.deserialize(bytes)
        if cu.classes.nonEmpty then
          inputs += PyLinker.Input(cu.classes, cu.mainEntry, source)
      catch
        case e: PyIRException =>
          System.err.println(s"[scalapy] warning: skipping ${child.getName}: ${e.getMessage}")
        case e: java.io.IOException =>
          System.err.println(s"[scalapy] warning: cannot read ${child.getName}: ${e.getMessage}")
