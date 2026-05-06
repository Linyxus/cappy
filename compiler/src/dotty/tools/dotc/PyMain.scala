package dotty.tools
package dotc

import java.io.{File, IOException}
import java.net.{URI, URL}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.collection.mutable

/** Launcher for the Python-backend compiler.
 *
 *  Wraps `dotty.tools.dotc.Main` so that `cs launch scpyc` (or any direct
 *  invocation) automatically:
 *
 *    1. fetches the support jars (`scala-library-py`, `scala-pylib-py`) into
 *       `~/.cache/scpyc/<version>/` from the gh-pages Maven repo,
 *    2. prepends them to the user-supplied `-classpath` (or adds a
 *       `-classpath` arg if none was given),
 *    3. injects `-scalapy` so the backend is enabled.
 *
 *  Why the support jars are downloaded out-of-band rather than declared as
 *  app-descriptor dependencies: their classes overlap with `scala-stdlib-py`
 *  (the JVM-runtime stdlib the compiler itself uses). Putting both on the
 *  JVM classpath causes runtime conflicts (e.g. mutual recursion between two
 *  `scala.runtime.BoxesRunTime` versions). They must live on the *compile*
 *  classpath only.
 */
object PyMain:

  private val baseUrl =
    "https://linyxus.github.io/scala3-py/maven/io/github/linyxus/scalapy"

  /** Module names of the support jars to fetch, in the order they should
   *  appear on the compile classpath.
   */
  private val supportArtifactIds = Seq("scala-library-py_3", "scala-pylib-py_3")

  def main(args: Array[String]): Unit =
    val version = config.Properties.versionNumberString
    if version.isEmpty then
      throw new RuntimeException("PyMain: cannot determine compiler version from compiler.properties")
    val supportJars = ensureSupportJars(version)
    Main.main(rewriteArgs(args, supportJars))
  end main

  private def ensureSupportJars(version: String): Seq[Path] =
    val cacheDir = Paths.get(sys.props("user.home"), ".cache", "scpyc", version)
    Files.createDirectories(cacheDir)
    supportArtifactIds.map { artifactId =>
      val jarName = s"$artifactId-$version.jar"
      val target = cacheDir.resolve(jarName)
      if !Files.exists(target) then
        val url = URI.create(s"$baseUrl/$artifactId/$version/$jarName").toURL
        System.err.println(s"PyMain: fetching $jarName ...")
        downloadTo(url, target)
      target
    }
  end ensureSupportJars

  private def downloadTo(url: URL, target: Path): Unit =
    val tmp = Files.createTempFile(target.getParent, ".download-", ".tmp")
    try
      val conn = url.openConnection()
      conn.setConnectTimeout(15_000)
      conn.setReadTimeout(120_000)
      val in = conn.getInputStream
      try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case e: IOException =>
        Files.deleteIfExists(tmp)
        throw new RuntimeException(s"PyMain: failed to download $url", e)
  end downloadTo

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
