package dotty
package tools
package dotc

import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.vulpix.TestConfiguration

import org.junit.Assert.*
import org.junit.Test
import org.junit.experimental.categories.Category

import java.io.{PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

@Category(Array(classOf[ScalaPyDceTest]))
class ScalaPyDceTest:

  @Test def helloBundleStaysSmallAndExcludesKnownDeadStdlibClasses(): Unit =
    withTempDir("scalapy-dce-hello") { outDir =>
      compileScalaPy("tests/pos-py/hello.scala", outDir)
      val bundle = outDir.resolve("hello.py")
      assertTrue(s"Expected generated bundle at $bundle", Files.exists(bundle))

      val text = Files.readString(bundle, StandardCharsets.UTF_8)
      val size = Files.size(bundle)
      val classCount = text.linesIterator.count(_.startsWith("class "))

      assertTrue(s"hello.py is too large: $size bytes", size < 2_200_000L)
      assertTrue(s"hello.py has too many classes: $classCount", classCount < 700)
      assertFalse(text.contains("scala_collection_mutable_BitSet"))
      assertFalse(text.contains("scala_collection_mutable_HashMap_HashMapIterator"))
      assertFalse(text.contains("scala_collection_immutable_BitSet"))
    }

  @Test def staleOutputDirPyirDoesNotBecomeAUserRoot(): Unit =
    withTempDir("scalapy-dce-stale") { outDir =>
      compileScalaPy("tests/py-dce/stale-probe/unused.scala", outDir, irOnly = true)
      compileScalaPy("tests/py-dce/stale-probe/main.scala", outDir)

      val bundle = outDir.resolve("main.py")
      assertTrue(s"Expected generated bundle at $bundle", Files.exists(bundle))
      val text = Files.readString(bundle, StandardCharsets.UTF_8)
      assertFalse(text.contains("StaleUnusedDceProbe"))
    }

  private def compileScalaPy(source: String, outDir: Path, irOnly: Boolean = false): Unit =
    val writer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(writer))
    val baseFlags = TestConfiguration.scalaPyOptions.withClasspath(outDir.toString)
    val flags = if irOnly then baseFlags.and("-scpy-ir-only") else baseFlags
    val args = flags.all ++ Array(
      "-d", outDir.toString,
      source
    )
    val result = Main.process(args, reporter, null)
    assertFalse(
      s"Compilation failed for $source\n${writer.toString}",
      result.hasErrors
    )

  private def withTempDir(prefix: String)(body: Path => Unit): Unit =
    val dir = Files.createTempDirectory(prefix)
    try body(dir)
    finally deleteRecursively(dir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream.iterator().asScala.toList
          .sortBy(_.getNameCount)
          .reverse
          .foreach(p => Files.deleteIfExists(p))
      finally stream.close()
