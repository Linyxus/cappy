package dotty.tools.cappyrepl

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Startup glue for the self-contained, released `cappy-repl` app.
 *
 *  The REPL driver compiles user input against a *compile* classpath of three
 *  support jars (`scala-library-bootstrapped`, `scala-pylib-py`,
 *  `scala-library-py`), passed via the `cappy.classpath` system property. Under
 *  `sbt run`/`sbt test` that property is set from freshly built jars. For the
 *  released `cappy-repl_3` app there is no sbt and no repo, so those jars are
 *  bundled inside the launcher jar under `/scpy/` (see the `resourceGenerator`
 *  in `project/Build.scala`).
 *
 *  These jars must stay on dotc's *compile* classpath only, never on the JVM
 *  classpath — `scala-library-py`/`scala-pylib-py` overlap with the
 *  `scala-stdlib-py` the compiler itself runs on, and putting both on JVM cp
 *  causes runtime conflicts. That is exactly why they are extracted to a cache
 *  dir and fed to dotc via `cappy.classpath` rather than declared as POM deps.
 */
object Launcher:

  /** (cache-friendly artifact name, jar resource path inside this launcher). */
  private val supportJars = Seq(
    "scala-library-bootstrapped" -> "/scpy/scala-library-bootstrapped.jar",
    "scala-pylib-py"             -> "/scpy/scala-pylib-py.jar",
    "scala-library-py"           -> "/scpy/scala-library-py.jar",
  )

  /** Ensure `cappy.classpath` points at the support jars.
   *
   *  No-op when the property is already set — that is the dev/test path,
   *  where sbt supplies freshly built jars via javaOptions. Otherwise (the
   *  released app) extract the bundled jars into `~/.cache/cappy-repl/<version>/`
   *  and set the property to their paths.
   */
  def ensureSupportClasspath(): Unit =
    if System.getProperty("cappy.classpath", "").nonEmpty then return

    val version = dotty.tools.dotc.config.Properties.versionNumberString
    if version.isEmpty then
      throw new RuntimeException(
        "cappy-repl launcher: cannot determine compiler version from compiler.properties")

    val cacheDir = Paths.get(sys.props("user.home"), ".cache", "cappy-repl", version)
    Files.createDirectories(cacheDir)
    val paths = supportJars.map { (name, resource) =>
      val target = cacheDir.resolve(s"$name-$version.jar")
      if !Files.exists(target) then extractTo(resource, target)
      target
    }
    System.setProperty(
      "cappy.classpath",
      paths.map(_.toAbsolutePath.toString).mkString(File.pathSeparator))
  end ensureSupportClasspath

  private def extractTo(resource: String, target: Path): Unit =
    val in = getClass.getResourceAsStream(resource)
    if in == null then
      throw new RuntimeException(
        s"cappy-repl launcher: bundled resource $resource missing from this jar — " +
        "set -Dcappy.classpath explicitly when running outside the released app")
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

end Launcher
