package scala

import scala.python.{PyAny, extern, name, native}

/** Minimal Predef for the Python backend.
 *
 *  Method signatures must match upstream `scala.Predef` so that linker
 *  resolution works — user code is typechecked against the upstream Predef
 *  from `scala-library-bootstrapped`, and at link time the Python linker
 *  resolves the encoded method names against this compiled version.
 *
 *  `println`/`print` route to Python's `builtins.print` via an
 *  `@extern` facade rather than through `java.io.PrintStream`.
 */
object Predef:
  // ---- Type aliases (must match upstream) ----

  type String    = java.lang.String
  type Class[T]  = java.lang.Class[T]

  // ---- Console output ----

  @extern("builtins")
  private object _builtins extends PyAny:
    @name("print")
    def pyPrint(x: Any): Unit = native

  // Python's `print` adds a trailing newline by default, matching `println`.
  def println(x: Any): Unit = _builtins.pyPrint(x)
  def println(): Unit = _builtins.pyPrint("")
  def print(x: Any): Unit = _builtins.pyPrint(x)

  // ---- Assertions ----

  def assert(assertion: Boolean): Unit =
    if !assertion then throw new AssertionError("assertion failed")

  def assert(assertion: Boolean, message: => Any): Unit =
    if !assertion then throw new AssertionError("assertion failed: " + message)

  def require(requirement: Boolean): Unit =
    if !requirement then throw new IllegalArgumentException("requirement failed")

  def require(requirement: Boolean, message: => Any): Unit =
    if !requirement then throw new IllegalArgumentException("requirement failed: " + message)

  // ---- Identity / locally ----

  def identity[A](x: A): A = x
  def locally[T](x: T): T = x

  // ---- Miscellaneous utilities used by compiler-generated code ----

  def ??? : Nothing = throw new NotImplementedError
  def classOf[T]: Class[T] = null.asInstanceOf[Class[T]]
  def valueOf[T]: T = null.asInstanceOf[T]
