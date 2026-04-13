package dotty.tools.dotc

import java.io.File
import java.nio.charset.StandardCharsets

import dotty.tools.vulpix.Status

/** Executes a bundled Python file and captures its output. */
object PyRun:

  def runPyCode(classPath: String): Status =
    // The classPath is a colon-separated list; the first entry is the output directory
    val outDir = new File(classPath.split(File.pathSeparator).head)
    val pyFiles = outDir.listFiles().filter(f => f.getName.endsWith(".py"))

    if pyFiles.isEmpty then
      return Status.Failure("No .py files found in output directory: " + outDir)

    // Pick the bundled .py file (there should be exactly one after linking)
    val pyFile = pyFiles.head

    try
      val process = new ProcessBuilder("python3", pyFile.getAbsolutePath)
        .directory(outDir)
        .redirectErrorStream(true)
        .start()

      val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      val exitCode = process.waitFor()

      if exitCode == 0 then Status.Success(output)
      else Status.Failure(output)
    catch
      case e: Exception =>
        Status.Failure(s"Failed to run Python: ${e.getMessage}")
