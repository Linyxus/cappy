package java.lang

/** Minimal `java.lang.Number` for the Python backend.
 *
 *  `Integer` and future boxed-number ports extend this instead of relying on
 *  the linker runtime whitelist.
 */
abstract class Number extends Object:
  def intValue(): scala.Int
  def longValue(): scala.Long
  def floatValue(): scala.Float
  def doubleValue(): scala.Double

  def byteValue(): scala.Byte =
    intValue().toByte

  def shortValue(): scala.Short =
    intValue().toShort
