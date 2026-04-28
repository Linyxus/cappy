package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Public wrapper for Python's `builtins` module.
 *
 *  Top-level builtins go through a private `@extern("builtins")`
 *  `PyDynamic` so keyword arguments and Python's overload-like
 *  polymorphism ("int(x)" vs "int(s, base)") just work.
 *
 *  `str` instance methods are exposed via a tiny `PyStr` facade: since
 *  Scala `String` and Python `str` share the same runtime object, we
 *  cast `String.asInstanceOf[PyStr]` at the bridge and invoke the
 *  method directly — no PyDynamic/`__call__` gymnastics.
 */
object PyBuiltins:
  @extern("builtins")
  private object builtins extends PyDynamic

  @extern("builtins", "float")
  private object floatType extends PyDynamic

  @extern("builtins", "bytes")
  private def bytesOf(value: Any): Any = native

  @extern("builtins", "float")
  private class PyFloat extends PyAny:
    def hex(): String = native

  @extern("builtins", "bytes")
  private class PyBytes extends PyAny:
    def decode(encoding: String): String = native

  @extern("operator", "getitem")
  private def getItem(value: Any, key: Any): Any = native

  @extern("builtins", "slice")
  private def sliceOf(start: Int, stop: Int): Any = native

  @extern("operator", "eq")
  private def operatorEq(a: Any, b: Any): Boolean = native

  /** Facade over Python's `str` for method dispatch. Scala `String`
   *  erases to Python `str` so the `asInstanceOf[PyStr]` bridges are
   *  no-ops at runtime. */
  @extern("builtins", "str")
  private class PyStr extends PyAny:
    def isalpha(): Boolean = native
    def isdigit(): Boolean = native
    def isalnum(): Boolean = native
    def isspace(): Boolean = native
    def isupper(): Boolean = native
    def islower(): Boolean = native
    def isascii(): Boolean = native
    def isdecimal(): Boolean = native
    def isnumeric(): Boolean = native
    def isprintable(): Boolean = native
    def upper(): String = native
    def lower(): String = native
    def swapcase(): String = native
    def capitalize(): String = native
    def title(): String = native
    def strip(): String = native
    def lstrip(): String = native
    def rstrip(): String = native
    def encode(encoding: String): PyAny = native
    def join(it: Any): String = native

  private inline def asStr(s: String): PyStr = s.asInstanceOf[PyStr]
  private inline def asFloat(value: Double): PyFloat = value.asInstanceOf[PyFloat]
  private inline def asBytes(value: Any): PyBytes = value.asInstanceOf[PyBytes]

  // --- Numeric formatting -------------------------------------------

  def hex_of(value: Long): String =
    builtins.hex(value).asInstanceOf[String]

  def oct_of(value: Long): String =
    builtins.oct(value).asInstanceOf[String]

  def bin_of(value: Long): String =
    builtins.bin(value).asInstanceOf[String]

  def chr_of(codePoint: Int): String =
    builtins.chr(codePoint).asInstanceOf[String]

  /** Bulk-build a Scala/Python `str` from a slice of an `Array[Char]`.
   *  Compiles to one Scala→Python boundary crossing — the iteration,
   *  per-codepoint `chr`, and `str` concatenation all happen in
   *  CPython's C-level `str.join(map(...))`. The original
   *  `var out = ""; out += chr(c); ...` loop in `String.new(Array[Char])`
   *  was the StringBuilder.toString hot path. */
  def string_from_chars(value: Array[Char], offset: Int, count: Int): String =
    val end = offset + count
    val src: Any =
      if offset == 0 && end == value.length then value
      else getItem(value, sliceOf(offset, end))
    asStr("").join(builtins.map(builtins.chr, src))

  def ord_of(c: String): Int =
    builtins.ord(c).asInstanceOf[Int]

  def equal(a: Any, b: Any): Boolean =
    operatorEq(a, b)

  // --- Parsing ------------------------------------------------------

  def int_parse(text: String): Long =
    builtins.int(text).asInstanceOf[Long]

  def int_parse(text: String, base: Int): Long =
    builtins.int(text, base).asInstanceOf[Long]

  def float_parse(text: String): Double =
    builtins.float(text).asInstanceOf[Double]

  def float_from_hex(text: String): Double =
    floatType.fromhex(text).asInstanceOf[Double]

  def float_hex(value: Double): String =
    asFloat(value).hex()

  // --- Numeric ops (typed variants of Python's polymorphic builtins) -

  def abs_int(x: Int): Int =
    builtins.abs(x).asInstanceOf[Int]

  def abs_long(x: Long): Long =
    builtins.abs(x).asInstanceOf[Long]

  def abs_double(x: Double): Double =
    builtins.abs(x).asInstanceOf[Double]

  def min_int(a: Int, b: Int): Int =
    builtins.min(a, b).asInstanceOf[Int]

  def min_long(a: Long, b: Long): Long =
    builtins.min(a, b).asInstanceOf[Long]

  def min_double(a: Double, b: Double): Double =
    builtins.min(a, b).asInstanceOf[Double]

  def max_int(a: Int, b: Int): Int =
    builtins.max(a, b).asInstanceOf[Int]

  def max_long(a: Long, b: Long): Long =
    builtins.max(a, b).asInstanceOf[Long]

  def max_double(a: Double, b: Double): Double =
    builtins.max(a, b).asInstanceOf[Double]

  /** Python's `round(x)` — banker's (half-to-even) rounding. Returns
   *  int for zero-digit form. Not the same as Java `Math.round`. */
  def round_to_long(x: Double): Long =
    builtins.round(x).asInstanceOf[Long]

  /** Python's `round(x, digits)` — returns float. */
  def round_double(x: Double, digits: Int): Double =
    builtins.round(x, digits).asInstanceOf[Double]

  /** Python's `len(obj)`. Works on str, list, dict, tuple, bytes, etc. */
  def length_of(obj: Any): Int =
    builtins.len(obj).asInstanceOf[Int]

  // --- `str` instance-method helpers --------------------------------

  def is_alpha(s: String): Boolean     = asStr(s).isalpha()
  def is_digit(s: String): Boolean     = asStr(s).isdigit()
  def is_alnum(s: String): Boolean     = asStr(s).isalnum()
  def is_space(s: String): Boolean     = asStr(s).isspace()
  def is_upper(s: String): Boolean     = asStr(s).isupper()
  def is_lower(s: String): Boolean     = asStr(s).islower()
  def is_ascii(s: String): Boolean     = asStr(s).isascii()
  def is_decimal(s: String): Boolean   = asStr(s).isdecimal()
  def is_numeric(s: String): Boolean   = asStr(s).isnumeric()
  def is_printable(s: String): Boolean = asStr(s).isprintable()

  def to_upper(s: String): String   = asStr(s).upper()
  def to_lower(s: String): String   = asStr(s).lower()
  def swap_case(s: String): String  = asStr(s).swapcase()
  def capitalize(s: String): String = asStr(s).capitalize()
  def title_case(s: String): String = asStr(s).title()
  def strip(s: String): String      = asStr(s).strip()
  def lstrip(s: String): String     = asStr(s).lstrip()
  def rstrip(s: String): String     = asStr(s).rstrip()

  /** Python `str.encode(encoding)` returns `bytes`. */
  def encode(s: String, encoding: String): PyAny =
    asStr(s).encode(encoding)

  /** Encode a Scala/Python string to a signed-byte Array[Byte]. */
  def encode_bytes(s: String, encoding: String): Array[Byte] =
    val raw = asBytes(asStr(s).encode(encoding))
    val len = builtins.len(raw).asInstanceOf[Int]
    val out = new Array[Byte](len)
    var i = 0
    while i < len do
      val b = getItem(raw, i).asInstanceOf[Int]
      out(i) = (if b >= 128 then b - 256 else b).toByte
      i += 1
    out

  /** Decode a signed-byte Array[Byte] through Python's `bytes.decode`. */
  def decode_bytes(bytes: Array[Byte], encoding: String): String =
    val unsigned = new Array[Int](bytes.length)
    var i = 0
    while i < bytes.length do
      unsigned(i) = bytes(i) & 0xFF
      i += 1
    asBytes(bytesOf(unsigned)).decode(encoding)
