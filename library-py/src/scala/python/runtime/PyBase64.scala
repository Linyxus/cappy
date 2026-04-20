package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's `base64` module. Drops the 170-line
 *  hand-rolled bit-fiddling port in `java.util.Base64` — Python's
 *  `base64` already implements RFC 4648 basic + URL-safe variants,
 *  so we route through it.
 *
 *  Inputs and outputs are byte-oriented. Scala `Array[Byte]` is a
 *  Python `list[int]` at runtime; the facade bridges those into
 *  `bytes` / `bytearray` at the Python boundary.
 */
object PyBase64:
  @extern("base64")
  private object base64 extends PyAny:
    @name("b64encode")
    def b64Encode(data: Any): PyDynamic = native
    @name("b64decode")
    def b64Decode(data: Any): PyDynamic = native
    @name("urlsafe_b64encode")
    def urlsafeEncode(data: Any): PyDynamic = native
    @name("urlsafe_b64decode")
    def urlsafeDecode(data: Any): PyDynamic = native

  @extern("builtins")
  private object builtins extends PyAny:
    @name("bytes")
    def newBytes(iterable: Any): PyDynamic = native
    @name("len")
    def lengthOf(container: Any): Int = native

  @extern("operator")
  private object operator extends PyAny:
    @name("getitem")
    def getItem(container: Any, index: Int): Any = native

  /** `Array[Byte]` → Python signed-byte payload → base64 via the
   *  requested alphabet. Caller decides whether to strip trailing
   *  `=` padding. */
  def encodeBasic(src: Array[Byte], padding: Boolean): Array[Byte] =
    doEncode(src, base64.b64Encode(toPyBytes(src)), padding)

  def encodeUrlSafe(src: Array[Byte], padding: Boolean): Array[Byte] =
    doEncode(src, base64.urlsafeEncode(toPyBytes(src)), padding)

  def decodeBasic(src: Array[Byte]): Array[Byte] =
    fromPyBytes(base64.b64Decode(toPyBytes(src)))

  def decodeUrlSafe(src: Array[Byte]): Array[Byte] =
    fromPyBytes(base64.urlsafeDecode(toPyBytes(src)))

  // --- byte bridges ---

  /** Scala Array[Byte] is a Python list of signed ints. Convert to a
   *  Python `bytes` object (unsigned 0..255) for base64's C impl. */
  private def toPyBytes(src: Array[Byte]): PyDynamic =
    val unsigned = new Array[Int](src.length)
    var i = 0
    while i < src.length do
      val b = src(i).toInt
      unsigned(i) = if b < 0 then b + 256 else b
      i += 1
    builtins.newBytes(unsigned)

  /** Python `bytes` → Scala `Array[Byte]` (signed). */
  private def fromPyBytes(src: PyDynamic): Array[Byte] =
    val len = builtins.lengthOf(src)
    val out = new Array[Byte](len)
    var i = 0
    while i < len do
      val u = operator.getItem(src, i).asInstanceOf[Int]
      out(i) = (if u >= 128 then u - 256 else u).toByte
      i += 1
    out

  /** Decorate the raw base64 bytes: strip trailing '=' padding if the
   *  encoder was asked not to pad. */
  private def doEncode(src: Array[Byte], encoded: PyDynamic, padding: Boolean): Array[Byte] =
    val raw = fromPyBytes(encoded)
    if padding then raw
    else stripPadding(raw)

  private def stripPadding(src: Array[Byte]): Array[Byte] =
    var end = src.length
    while end > 0 && src(end - 1) == 61.toByte do
      end -= 1
    if end == src.length then src
    else
      val out = new Array[Byte](end)
      var i = 0
      while i < end do
        out(i) = src(i)
        i += 1
      out
