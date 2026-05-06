package dotty.tools
package dotc

import java.io.{File, InputStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.collection.mutable

/** Launcher for the Python-backend compiler.
 *
 *  Wraps `dotty.tools.dotc.Main` so that `cs launch spc` (or any direct
 *  invocation) automatically:
 *
 *    1. extracts the support jars (`scala-library-py`, `scala-pylib-py`),
 *       which are bundled inside this jar under `/scpy/`, into
 *       `~/.cache/spc/<version>/`,
 *    2. prepends them to the user-supplied `-classpath` (or adds a
 *       `-classpath` arg if none was given),
 *    3. injects `-scalapy` so the backend is enabled.
 *
 *  Why the support jars are bundled inside this launcher rather than declared
 *  as app-descriptor dependencies: their classes overlap with `scala-stdlib-py`
 *  (the JVM-runtime stdlib the compiler itself uses). Putting both on the JVM
 *  classpath causes runtime conflicts (mutual recursion between two
 *  `scala.runtime.BoxesRunTime` versions). They must live on the *compile*
 *  classpath only — never on JVM cp.
 */
object PyMain:

  /** (cache-friendly artifact name, jar resource path inside this launcher). */
  private val supportJars = Seq(
    "scala-library-py" -> "/scpy/scala-library-py.jar",
    "scala-pylib-py"   -> "/scpy/scala-pylib-py.jar",
  )

  def main(args: Array[String]): Unit =
    val version = config.Properties.versionNumberString
    if version.isEmpty then
      throw new RuntimeException(
        "PyMain: cannot determine compiler version from compiler.properties")
    val supportPaths = extractSupportJars(version)
    Main.main(rewriteArgs(args, supportPaths))
  end main

  private def extractSupportJars(version: String): Seq[Path] =
    val cacheDir = Paths.get(sys.props("user.home"), ".cache", "spc", version)
    Files.createDirectories(cacheDir)
    supportJars.map { (name, resource) =>
      val target = cacheDir.resolve(s"$name-$version.jar")
      if !Files.exists(target) then extractTo(resource, target)
      target
    }
  end extractSupportJars

  private def extractTo(resource: String, target: Path): Unit =
    val in = getClass.getResourceAsStream(resource)
    if in == null then
      throw new RuntimeException(
        s"PyMain: bundled resource $resource missing from this launcher jar")
    val tmp = Files.createTempFile(target.getParent, ".extract-", ".tmp")
    try
      try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case e: Throwable =>
        Files.deleteIfExists(tmp)
        throw e
  end extractTo

  private def rewriteArgs(args: Array[String], supportJars: Seq[Path]): Array[String] =
    val supportCp = supportJars.map(_.toAbsolutePath.toString).mkString(File.pathSeparator)
    val needScalaPy = !args.contains("-scalapy")
    val buf = mutable.ArrayBuffer.empty[String]
    var i = 0
    var injectedCp = false
    while i < args.length do
      val a = args(i)
      if (a == "-classpath" || a == "-cp") && i + 1 < args.length then
        val userCp = args(i + 1)
        val merged =
          if userCp.nonEmpty then s"$userCp${File.pathSeparator}$supportCp"
          else supportCp
        buf += a
        buf += merged
        i += 2
        injectedCp = true
      else
        buf += a
        i += 1
    if !injectedCp then
      buf.prepend(supportCp)
      buf.prepend("-classpath")
    if needScalaPy then buf.prepend("-scalapy")
    buf.toArray
  end rewriteArgs

end PyMain
