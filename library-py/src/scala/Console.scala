package scala

/** Minimal Console stub for the Python backend.
 *
 *  Routes through `Predef.println` which reaches Python's `builtins.print`.
 */
object Console:
  def println(x: Any): Unit = Predef.println(x)
  def println(): Unit = Predef.println()
  def print(x: Any): Unit = Predef.print(x)
