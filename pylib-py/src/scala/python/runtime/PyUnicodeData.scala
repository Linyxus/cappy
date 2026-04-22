package scala.python.runtime

import scala.python.{extern, native}

/** Thin facade over Python's `unicodedata` module for the Character port. */
object PyUnicodeData:
  @extern("unicodedata", "category")
  private def pyCategory(ch: String): String = native

  @extern("unicodedata", "bidirectional")
  private def pyBidirectional(ch: String): String = native

  @extern("unicodedata", "mirrored")
  private def pyMirrored(ch: String): Int = native

  @extern("unicodedata", "decimal")
  private def pyDecimal(ch: String): Int = native

  @extern("unicodedata", "numeric")
  private def pyNumeric(ch: String): Double = native

  def category(ch: String): String =
    pyCategory(ch)

  def bidirectional(ch: String): String =
    pyBidirectional(ch)

  def mirrored(ch: String): Int =
    pyMirrored(ch)

  def decimalOrMinusOne(ch: String): Int =
    try pyDecimal(ch)
    catch case _: Throwable => -1

  def numericOrNaN(ch: String): Double =
    try pyNumeric(ch)
    catch case _: Throwable => Double.NaN
