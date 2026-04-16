package scala.util

/** Minimal CommandLineParser stub for the Python backend.
 *
 *  The compiler's `@main` lowering generates calls to
 *  `CommandLineParser.showError` for parse failures.
 */
object CommandLineParser:
  class ParseError(msg: String) extends Exception(msg)

  def showError(error: ParseError): Unit =
    Predef.println(error.toString)
