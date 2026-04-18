package scala.python.runtime

import scala.python.{extern, native}

/** Minimal bit-level helpers for `java.lang.Math` float stepping. */
object PyStruct:
  @extern("struct", "pack")
  private def pyPack(format: String, value: Any): Any = native

  @extern("struct", "unpack")
  private def pyUnpack(format: String, buffer: Any): Any = native

  @extern("operator", "getitem")
  private def pyGetItem(value: Any, index: Int): Any = native

  def float_to_int32_bits(value: Float): Int =
    pyGetItem(pyUnpack(">i", pyPack(">f", value)), 0).asInstanceOf[Int]

  def float_from_int32_bits(bits: Int): Float =
    pyGetItem(pyUnpack(">f", pyPack(">i", bits)), 0).asInstanceOf[Float]
