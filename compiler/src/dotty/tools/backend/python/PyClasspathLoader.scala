package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.serialization.{PyIRDeserializer, PyIRException}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report

import java.io.File
import java.util.jar.JarFile
import scala.collection.mutable

/** Walks the compile classpath AND the compile's output directory for
 *  `.pyir` entries and deserializes them into `Support`-tagged
 *  [[PyLinker.Input]] values.
 *
 *  Roots of the reachability check are the classes produced by the
 *  current compile, handed to the linker in-memory by
 *  [[dotty.tools.backend.python.GenPython]] — this loader does **not**
 *  produce `User` inputs. Any `.pyir` found in the output directory is
 *  leftover state from a previous compile (or the output-dir-on-classpath
 *  case) and is treated as Support, subject to DCE.
 *
 *  Mirrors Scala.js's `PathIRContainer.fromClasspath` + the `scalaJSIR`
 *  sbt task, which reads `.sjsir` via the `fullClasspath` (which always
 *  includes the compile output). See
 *  `inbox/scala-js/linker/jvm/src/main/scala/org/scalajs/linker/PathIRContainer.scala`.
 *
 *  Results are deduped by canonical file path so that `.pyir` reachable
 *  both via the output directory and via an overlapping classpath entry
 *  (e.g. default `-cp .` + default `-d .`) is only loaded once.
 *
 *  Diagnostics for corrupt/unreadable `.pyir` are routed through the
 *  compiler [[report]] facility. Failures inside the ScalaPy support
 *  classpath entries (the `scala-pylib-py` / `scala-library-py` artifacts)
 *  are reported as hard errors, since a missing support symbol almost
 *  always cascades into confusing secondary link errors. Failures in any
 *  other classpath entry are reported as warnings.
 */
object PyClasspathLoader:

  /** Substrings that identify a classpath entry as a required ScalaPy
   *  support input. Both the packaged jar names and the raw class
   *  directory paths contain these tokens (the sbt project names),
   *  so a canonical-path substring check is robust.
   */
  private val SupportEntryMarkers: List[String] =
    List("scala-pylib-py", "scala-library-py")

  /** Load every `.pyir` on the classpath (and any left over at the top
   *  level of the output dir), tagged [[PyLinker.InputSource.Support]].
   *
   *  @param excludeOutputFile  canonical file whose content should be
   *                            skipped — typically the `.pyir` the
   *                            current compile just wrote, since the
   *                            in-memory User input already covers it.
   */
  def loadSupportInputs(excludeOutputFile: Option[File] = None)(using ctx: Context): List[PyLinker.Input] =
    val cp = ctx.settings.classpath.value
    val outDir = outputDirCanonical
    val inputs = mutable.ListBuffer.empty[PyLinker.Input]
    val visitedFiles = mutable.HashSet.empty[String]

    // Pre-register the canonical path of the just-written User `.pyir`
    // so the subsequent scans skip it outright.
    excludeOutputFile.foreach { f =>
      val canon =
        try f.getCanonicalPath.nn catch case _: java.io.IOException => f.getAbsolutePath.nn
      visitedFiles += canon
    }

    // Scan the output dir top-level first so an overlapping classpath
    // entry doesn't re-add the same file. Output-dir strays are
    // Support — they are NOT roots.
    //
    // The scan is NON-RECURSIVE: emission writes `<sourceName>.pyir`
    // directly at the top of the output dir, so subtrees (e.g. a
    // library's `target/.../classes/*.pyir` when `-d .` happens to equal
    // cwd) are not our output. They will be picked up by the classpath
    // scan below, so nothing is lost.
    //
    // The output directory is never treated as "required support":
    // strays here belong to a previous compile, not the support libs.
    outDir.foreach { dir =>
      val f = new File(dir)
      if f.isDirectory then
        loadFromDirTopLevel(f, PyLinker.InputSource.Support, required = false, visitedFiles, inputs)
    }

    for entry <- cp.split(File.pathSeparator) do
      val f = new File(entry)
      val required = isSupportEntry(f)
      if f.isFile && f.getName.endsWith(".jar") then
        loadFromJar(f, PyLinker.InputSource.Support, required, visitedFiles, inputs)
      else if f.isDirectory then
        loadFromDir(f, PyLinker.InputSource.Support, required, visitedFiles, inputs)

    inputs.toList

  private def outputDirCanonical(using ctx: Context): Option[String] =
    val out = ctx.settings.outputDir.value
    val jpath = out.jpath
    if jpath == null then None
    else try Some(jpath.toFile.getCanonicalPath.nn) catch case _: java.io.IOException => None

  /** True if `entry` is one of the ScalaPy support classpath inputs.
   *  Matched by canonical-path substring against the sbt project names
   *  (`scala-pylib-py`, `scala-library-py`), which appear both in the
   *  packaged jar filenames and in the raw class-directory paths.
   */
  private def isSupportEntry(entry: File): Boolean =
    val path =
      try entry.getCanonicalPath.nn catch case _: java.io.IOException => entry.getAbsolutePath.nn
    SupportEntryMarkers.exists(path.contains)

  /** Describe the original throwable for inclusion in a diagnostic. */
  private def describeCause(e: Throwable): String =
    val msg = e.getMessage
    if msg == null || msg.isEmpty then e.getClass.getName.nn
    else s"${e.getClass.getName}: $msg"

  /** Report a corrupt/unreadable `.pyir` through the compiler reporter.
   *  Required (support) entries become hard errors; everything else is
   *  a warning.
   */
  private def reportLoadFailure(file: File, required: Boolean, e: Throwable)(using Context): Unit =
    val path =
      try file.getCanonicalPath.nn catch case _: java.io.IOException => file.getAbsolutePath.nn
    val cause = describeCause(e)
    if required then
      report.error(
        s"[scalapy] cannot load required ScalaPy support classpath entry $path: $cause. " +
        s"This usually means the support libraries are stale or built against an incompatible " +
        s"PyIR format; rebuild scala-pylib-py and scala-library-py."
      )
    else
      report.warning(s"[scalapy] skipping classpath entry $path: $cause")

  private def loadFromJar(
      jarFile: File,
      source: PyLinker.InputSource,
      required: Boolean,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  )(using Context): Unit =
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
      case e: PyIRException        => reportLoadFailure(jarFile, required, e)
      case e: java.io.IOException  => reportLoadFailure(jarFile, required, e)

  private def loadFromDir(
      dir: File,
      source: PyLinker.InputSource,
      required: Boolean,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  )(using Context): Unit =
    def walk(d: File): Unit =
      val children = d.listFiles()
      if children != null then
        for child <- children do
          if child.isDirectory then walk(child)
          else if child.getName.endsWith(".pyir") then
            ingest(child, source, required, visitedFiles, inputs)
    walk(dir)

  /** Scan only the direct contents of `dir`, not subdirectories.
   *  Used for the output-directory scan: emitted `.pyir` files live
   *  flat at the top of the output dir, and recursing would pick up
   *  unrelated trees when the output dir overlaps the classpath. */
  private def loadFromDirTopLevel(
      dir: File,
      source: PyLinker.InputSource,
      required: Boolean,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  )(using Context): Unit =
    val children = dir.listFiles()
    if children != null then
      for child <- children do
        if !child.isDirectory && child.getName.endsWith(".pyir") then
          ingest(child, source, required, visitedFiles, inputs)

  private def ingest(
      child: File,
      source: PyLinker.InputSource,
      required: Boolean,
      visitedFiles: mutable.HashSet[String],
      inputs: mutable.ListBuffer[PyLinker.Input]
  )(using Context): Unit =
    val canon =
      try child.getCanonicalPath.nn catch case _: java.io.IOException => child.getAbsolutePath.nn
    if visitedFiles.add(canon) then
      try
        val bytes = java.nio.file.Files.readAllBytes(child.toPath)
        val cu = PyIRDeserializer.deserialize(bytes)
        if cu.classes.nonEmpty then
          inputs += PyLinker.Input(cu.classes, cu.mainEntry, source)
      catch
        case e: PyIRException       => reportLoadFailure(child, required, e)
        case e: java.io.IOException => reportLoadFailure(child, required, e)
