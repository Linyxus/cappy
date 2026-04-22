package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyInt:
  @extern("builtins")
  private object builtins extends PyAny:
    @name("int")
    def parseString(text: String, radix: Int): PyDynamic = native

    @name("pow")
    def powMod(base: Any, exp: Any, mod: Any): PyDynamic = native

    @name("hash")
    def hashValue(value: Any): Any = native

  @extern("math")
  private object math extends PyAny:
    def gcd(a: Any, b: Any): PyDynamic = native

  @extern("__main__")
  private object runtime extends PyAny:
    @name("_scpy_i32")
    def toInt32(value: Any): Int = native

    @name("_scpy_i64")
    def toInt64(value: Any): Long = native

    @name("_scpy_f32")
    def toFloat32(value: Double): Float = native

    @name("_scpy_int_to_double")
    def toDoubleValue(value: Any): Double = native

    @name("_scpy_int_to_string_radix")
    def toStringRadix(value: Any, radix: Int): String = native

    @name("_scpy_int_to_signed_bytes")
    def toSignedBytes(value: Any): PyDynamic = native

    @name("_scpy_int_from_signed_bytes")
    def fromSignedBytes(bytes: Any): PyDynamic = native

    @name("_scpy_int_from_unsigned_bytes")
    def fromUnsignedBytes(bytes: Any): PyDynamic = native

    @name("_scpy_int_trunc_div")
    def truncDiv(a: Any, b: Any): PyDynamic = native

    @name("_scpy_int_signum")
    def signum(value: Any): Int = native

    @name("_scpy_int_compare")
    def compare(a: Any, b: Any): Int = native

    @name("_scpy_int_bit_length")
    def bitLength(value: Any): Int = native

    @name("_scpy_int_bit_count")
    def bitCount(value: Any): Int = native

    @name("_scpy_int_lowest_set_bit")
    def lowestSetBit(value: Any): Int = native

    @name("_scpy_int_test_bit")
    def testBit(value: Any, index: Int): Boolean = native

  def fromLong(value: Long): PyDynamic =
    builtins.parseString(value.toString, 10)

  def fromString(text: String, radix: Int): PyDynamic =
    builtins.parseString(text, radix)

  def fromSignedBytes(bytes: Array[Byte]): PyDynamic =
    runtime.fromSignedBytes(PyBytes.toPyBytes(bytes))

  def fromUnsignedBytes(bytes: Array[Byte]): PyDynamic =
    runtime.fromUnsignedBytes(PyBytes.toPyBytes(bytes))

  def toString(value: PyDynamic, radix: Int): String =
    runtime.toStringRadix(value, radix)

  def modPow(base: PyDynamic, exp: PyDynamic, mod: PyDynamic): PyDynamic =
    builtins.powMod(base, exp, mod)

  def modInverse(base: PyDynamic, mod: PyDynamic): PyDynamic =
    builtins.powMod(base, -1, mod)

  def gcd(a: PyDynamic, b: PyDynamic): PyDynamic =
    math.gcd(a, b)

  def bitCount(value: PyDynamic): Int =
    runtime.bitCount(value)

  def bitLength(value: PyDynamic): Int =
    runtime.bitLength(value)

  def lowestSetBit(value: PyDynamic): Int =
    runtime.lowestSetBit(value)

  def testBit(value: PyDynamic, index: Int): Boolean =
    runtime.testBit(value, index)

  def toSignedBytes(value: PyDynamic): Array[Byte] =
    PyBytes.fromPyBytes(runtime.toSignedBytes(value))

  def isZero(value: PyDynamic): Boolean =
    runtime.signum(value) == 0

  def signum(value: PyDynamic): Int =
    runtime.signum(value)

  def compare(a: PyDynamic, b: PyDynamic): Int =
    runtime.compare(a, b)

  /** Java-shape hashCode — truncates Python's platform-width `hash`
   *  into an Int. Equal values produce equal hashes; the exact bit
   *  pattern diverges from JDK's digit-array hash, but that's not
   *  observable through the `Object.hashCode()` contract. */
  def hashCode(value: PyDynamic): Int =
    runtime.toInt32(builtins.hashValue(value))

  def truncDiv(a: PyDynamic, b: PyDynamic): PyDynamic =
    runtime.truncDiv(a, b)

  def toInt(value: PyDynamic): Int =
    runtime.toInt32(value)

  def toLong(value: PyDynamic): Long =
    runtime.toInt64(value)

  def toDouble(value: PyDynamic): Double =
    runtime.toDoubleValue(value)

  def toFloat(value: PyDynamic): Float =
    runtime.toFloat32(toDouble(value))
