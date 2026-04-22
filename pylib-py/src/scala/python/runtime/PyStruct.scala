package scala.python.runtime

import scala.python.{extern, native}

/** IEEE 754 bit-level conversions between floating-point values and
 *  their integer bit patterns, plus raw pack/unpack helpers.
 *
 *  Used by `java.lang.Math` (for float stepping) and by the boxed-
 *  primitive ports `java.lang.{Float,Double}` (for `floatToIntBits`,
 *  `doubleToLongBits`, and inverses).
 *
 *  All conversions use big-endian (`>`) format strings. The result of
 *  `float_to_int32_bits(1.0f)` is 0x3F800000 (= 1065353216), matching
 *  Java's `Float.floatToRawIntBits(1.0f)`.
 */
object PyStruct:
  @extern("struct", "pack")
  private def pyPack(format: String, value: Any): Any = native

  @extern("struct", "unpack")
  private def pyUnpack(format: String, buffer: Any): Any = native

  @extern("operator", "getitem")
  private def pyGetItem(value: Any, index: Int): Any = native

  // --- IEEE 754 bit patterns ----------------------------------------

  def float_to_int32_bits(value: Float): Int =
    pyGetItem(pyUnpack(">i", pyPack(">f", value)), 0).asInstanceOf[Int]

  def float_from_int32_bits(bits: Int): Float =
    pyGetItem(pyUnpack(">f", pyPack(">i", bits)), 0).asInstanceOf[Float]

  def double_to_int64_bits(value: Double): Long =
    pyGetItem(pyUnpack(">q", pyPack(">d", value)), 0).asInstanceOf[Long]

  def double_from_int64_bits(bits: Long): Double =
    pyGetItem(pyUnpack(">d", pyPack(">q", bits)), 0).asInstanceOf[Double]

  // --- Raw pack / unpack (big-endian) -------------------------------
  // These return Python `bytes` objects (typed as `Any` since bytes
  // don't have a direct Scala type). Callers pass them to unpack, not
  // directly to Scala code.

  def pack_float_be(value: Float): Any =
    pyPack(">f", value)

  def pack_double_be(value: Double): Any =
    pyPack(">d", value)

  def unpack_float_be(buffer: Any): Float =
    pyGetItem(pyUnpack(">f", buffer), 0).asInstanceOf[Float]

  def unpack_double_be(buffer: Any): Double =
    pyGetItem(pyUnpack(">d", buffer), 0).asInstanceOf[Double]

  // --- Little-endian variants ---------------------------------------

  def pack_float_le(value: Float): Any =
    pyPack("<f", value)

  def pack_double_le(value: Double): Any =
    pyPack("<d", value)

  def unpack_float_le(buffer: Any): Float =
    pyGetItem(pyUnpack("<f", buffer), 0).asInstanceOf[Float]

  def unpack_double_le(buffer: Any): Double =
    pyGetItem(pyUnpack("<d", buffer), 0).asInstanceOf[Double]
