import scala.python.runtime.PyStruct

@main def pyfacadeStruct(): Unit =
  // --- Float: known IEEE 754 bit patterns ---------------------------
  println("f-zero:" + PyStruct.float_to_int32_bits(0.0f))
  println("f-one:" + PyStruct.float_to_int32_bits(1.0f))
  println("f-one-hex:" + (PyStruct.float_to_int32_bits(1.0f) == 0x3F800000))
  println("f-neg-one:" + (PyStruct.float_to_int32_bits(-1.0f) == 0xBF800000))
  println("f-two:" + (PyStruct.float_to_int32_bits(2.0f) == 0x40000000))
  println("f-half:" + (PyStruct.float_to_int32_bits(0.5f) == 0x3F000000))
  println("f-neg-zero:" + (PyStruct.float_to_int32_bits(-0.0f) == 0x80000000))
  println("f-pos-inf:" + (PyStruct.float_to_int32_bits(Float.PositiveInfinity) == 0x7F800000))
  println("f-neg-inf:" + (PyStruct.float_to_int32_bits(Float.NegativeInfinity) == 0xFF800000))

  // --- Float: round-trips -------------------------------------------
  println("f-rt-0:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(0.0f)) == 0.0f))
  println("f-rt-1:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(1.0f)) == 1.0f))
  println("f-rt-neg-1:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(-1.0f)) == -1.0f))
  println("f-rt-1.5:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(1.5f)) == 1.5f))
  println("f-rt-neg-2.5:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(-2.5f)) == -2.5f))
  println("f-rt-0.25:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(0.25f)) == 0.25f))
  println("f-rt-1e10:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(1e10f)) == 1e10f))
  println("f-rt-1048576:" + (PyStruct.float_from_int32_bits(PyStruct.float_to_int32_bits(1048576.0f)) == 1048576.0f))
  println("f-from-bits-one:" + PyStruct.float_from_int32_bits(0x3F800000))
  println("f-from-bits-two:" + PyStruct.float_from_int32_bits(0x40000000))

  // --- Float: NaN shape ---------------------------------------------
  val fNanBits = PyStruct.float_to_int32_bits(Float.NaN)
  val fNanExponent = (fNanBits >>> 23) & 0xFF
  val fNanMantissa = fNanBits & 0x7FFFFF
  println("f-nan-exp-all-ones:" + (fNanExponent == 0xFF))
  println("f-nan-mantissa-nonzero:" + (fNanMantissa != 0))

  // --- Double: known IEEE 754 bit patterns --------------------------
  println("d-zero:" + PyStruct.double_to_int64_bits(0.0))
  println("d-one:" + PyStruct.double_to_int64_bits(1.0))
  println("d-one-hex:" + (PyStruct.double_to_int64_bits(1.0) == 0x3FF0000000000000L))
  println("d-neg-one:" + (PyStruct.double_to_int64_bits(-1.0) == 0xBFF0000000000000L))
  println("d-two:" + (PyStruct.double_to_int64_bits(2.0) == 0x4000000000000000L))
  println("d-half:" + (PyStruct.double_to_int64_bits(0.5) == 0x3FE0000000000000L))
  println("d-neg-zero:" + (PyStruct.double_to_int64_bits(-0.0) == 0x8000000000000000L))
  println("d-pos-inf:" + (PyStruct.double_to_int64_bits(Double.PositiveInfinity) == 0x7FF0000000000000L))
  println("d-neg-inf:" + (PyStruct.double_to_int64_bits(Double.NegativeInfinity) == 0xFFF0000000000000L))

  // --- Double: round-trips ------------------------------------------
  println("d-rt-0:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(0.0)) == 0.0))
  println("d-rt-1:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(1.0)) == 1.0))
  println("d-rt-pi:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(3.141592653589793)) == 3.141592653589793))
  println("d-rt-e:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(2.718281828459045)) == 2.718281828459045))
  println("d-rt-1e-10:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(1e-10)) == 1e-10))
  println("d-rt-1e100:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(1e100)) == 1e100))
  println("d-rt-max:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(Double.MaxValue)) == Double.MaxValue))
  println("d-rt-min:" + (PyStruct.double_from_int64_bits(PyStruct.double_to_int64_bits(Double.MinValue)) == Double.MinValue))
  println("d-from-bits-one:" + PyStruct.double_from_int64_bits(0x3FF0000000000000L))

  // --- Double: NaN shape --------------------------------------------
  val dNanBits = PyStruct.double_to_int64_bits(Double.NaN)
  val dNanExponent = (dNanBits >>> 52) & 0x7FFL
  val dNanMantissa = dNanBits & 0xFFFFFFFFFFFFFL
  println("d-nan-exp-all-ones:" + (dNanExponent == 0x7FFL))
  println("d-nan-mantissa-nonzero:" + (dNanMantissa != 0L))

  // --- Bit-pattern ↔ value inverse -----------------------------------
  // Bit-manipulate a Float, round-trip, and check an expected change.
  val f3: Float = 3.0f
  val f3Bits = PyStruct.float_to_int32_bits(f3)
  // Doubling a float means incrementing the exponent by 1 — bits shift
  // upward by 0x00800000.
  val f6Bits = f3Bits + 0x00800000
  println("f-double-via-bits:" + PyStruct.float_from_int32_bits(f6Bits))

  val d3: Double = 3.0
  val d3Bits = PyStruct.double_to_int64_bits(d3)
  val d6Bits = d3Bits + 0x0010000000000000L
  println("d-double-via-bits:" + PyStruct.double_from_int64_bits(d6Bits))

  // --- Pack / unpack big-endian round-trips -------------------------
  val fBuf = PyStruct.pack_float_be(1.25f)
  println("be-f-rt-1.25:" + (PyStruct.unpack_float_be(fBuf) == 1.25f))
  val dBuf = PyStruct.pack_double_be(1.25)
  println("be-d-rt-1.25:" + (PyStruct.unpack_double_be(dBuf) == 1.25))

  val fBufNeg = PyStruct.pack_float_be(-7.5f)
  println("be-f-rt-neg-7.5:" + (PyStruct.unpack_float_be(fBufNeg) == -7.5f))
  val dBufNeg = PyStruct.pack_double_be(-7.5)
  println("be-d-rt-neg-7.5:" + (PyStruct.unpack_double_be(dBufNeg) == -7.5))

  // --- Pack / unpack little-endian round-trips ----------------------
  val fLe = PyStruct.pack_float_le(0.25f)
  println("le-f-rt-0.25:" + (PyStruct.unpack_float_le(fLe) == 0.25f))
  val dLe = PyStruct.pack_double_le(0.25)
  println("le-d-rt-0.25:" + (PyStruct.unpack_double_le(dLe) == 0.25))

  val fLeSpecial = PyStruct.pack_float_le(1048576.0f)
  println("le-f-rt-2pow20:" + (PyStruct.unpack_float_le(fLeSpecial) == 1048576.0f))
  val dLeSpecial = PyStruct.pack_double_le(1e50)
  println("le-d-rt-1e50:" + (PyStruct.unpack_double_le(dLeSpecial) == 1e50))

  // --- Endianness cross-check ---------------------------------------
  // big-endian and little-endian buffers for the same value differ
  // (except on palindromic bit patterns like 0.0).
  val oneBeBuf = PyStruct.pack_float_be(1.0f)
  val oneLeBuf = PyStruct.pack_float_le(1.0f)
  // Decoding BE-encoded bytes as LE yields a different float.
  println("le-decode-be-differs:" + (PyStruct.unpack_float_le(oneBeBuf) != 1.0f))
  println("be-decode-le-differs:" + (PyStruct.unpack_float_be(oneLeBuf) != 1.0f))
