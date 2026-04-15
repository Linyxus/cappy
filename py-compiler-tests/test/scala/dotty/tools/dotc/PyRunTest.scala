package dotty.tools.dotc

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Comparator

import scala.jdk.CollectionConverters.*

import dotty.tools.vulpix.Status

import org.junit.Assert.*
import org.junit.Test

class PyRunTest:

  private val repoRoot = new File(System.getProperty("user.dir")).getCanonicalFile

  @Test def runsGeneratedPythonThroughLockedUvProject(): Unit =
    withTempOutDir("pyrun-uv") { outDir =>
      val script =
        """|import sys
           |import numpy as np
           |
           |print(f"{sys.version_info.major}.{sys.version_info.minor}")
           |print(np.array([1, 2, 3]).tolist())
           |""".stripMargin

      Files.writeString(outDir.toPath.resolve("bundle.py"), script, StandardCharsets.UTF_8)

      PyRun.runPyCode(outDir.getAbsolutePath) match
        case Status.Success(output) =>
          val lines = output.linesIterator.toList
          assertEquals(List(projectPythonMajorMinor, "[1, 2, 3]"), lines)
        case Status.Failure(output) =>
          fail(s"Expected PyRun to execute successfully via uv, got failure:\n$output")
        case Status.Timeout =>
          fail("Expected PyRun to execute successfully via uv, but it timed out")
    }

  @Test def syncFailureMessageMentionsRecoveryCommand(): Unit =
    val message = PyRun.formatSyncFailure(repoRoot, "mock uv output")
    assertTrue(message.contains("uv sync --frozen"))
    assertTrue(message.contains("pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests"))
    assertTrue(message.contains("mock uv output"))

  private def projectPythonMajorMinor: String =
    val cfg = repoRoot.toPath.resolve(".venv").resolve("pyvenv.cfg")
    val versionInfo = Files.readAllLines(cfg, StandardCharsets.UTF_8).asScala.collectFirst {
      case line if line.startsWith("version_info = ") => line.stripPrefix("version_info = ").trim
    }.getOrElse(fail(s"Could not read version_info from ${cfg.toAbsolutePath}"))
    versionInfo.split('.').take(2).mkString(".")

  private def withTempOutDir[A](prefix: String)(body: File => A): A =
    val base = Files.createDirectories(repoRoot.toPath.resolve("target").resolve("tmp-pyrun-tests"))
    val dir = Files.createTempDirectory(base, prefix).toFile
    try body(dir)
    finally deleteRecursively(dir)

  private def deleteRecursively(file: File): Unit =
    if file.exists then
      val paths = Files.walk(file.toPath)
      try
        paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally
        paths.close()
