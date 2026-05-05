package dotty.tools.backend.python

/** Scratch main for Phase 4 byte-identity verification.
 *
 *  Writes `PyIRRuntime.content` to a file path so we can `diff` the
 *  prelude before and after each Phase 4 slice. Deleted in the
 *  final slice once the refactor lands.
 *
 *  Usage:
 *    sbt --client "scala3-compiler-bootstrapped/Test/runMain \
 *      dotty.tools.backend.python.DumpPreludeBytes /tmp/prelude.txt"
 */
object DumpPreludeBytes:
  def main(args: Array[String]): Unit =
    java.nio.file.Files.writeString(
      java.nio.file.Paths.get(args(0)),
      PyIRRuntime.content
    )
