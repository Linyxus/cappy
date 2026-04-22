package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's incremental codec machinery.
 *
 *  Wraps the codec-specific `IncrementalDecoder` class so that
 *  `InputStreamReader` can feed byte chunks to the decoder as they
 *  arrive from the underlying stream, without buffering the entire
 *  input first. Partial multi-byte sequences that straddle chunk
 *  boundaries are held inside the decoder until the next chunk
 *  arrives.
 */
object PyCodecs:
  @extern("codecs")
  private object codecs extends PyDynamic

  @extern("__main__")
  private object runtime extends PyAny:
    @name("_scpy_codec_lookup")
    def codecLookup(name: String): String | Null = native

    @name("_scpy_codec_decode_step")
    def codecDecodeStep(decoder: Any, data: PyDynamic, finalChunk: Boolean): PyDynamic = native

    @name("_scpy_codec_encode_step")
    def codecEncodeStep(encoder: Any, text: String, finalChunk: Boolean): PyDynamic = native

  @extern("builtins")
  private[runtime] object builtins extends PyAny:
    @name("hasattr")
    def hasAttr(obj: Any, name: String): Boolean = native

    @name("getattr")
    def getAttr(obj: Any, name: String): Any = native

    @name("len")
    def lengthOf(container: Any): Int = native

  @extern("operator")
  private[runtime] object operator extends PyAny:
    @name("getitem")
    def getItem(container: Any, index: Int): Any = native

  @extern("encodings.utf_8", "IncrementalDecoder")
  final class Utf8IncrementalDecoder extends PyAny:
    /** Decode a chunk of bytes. Pass `finalChunk = true` on the last
     *  call so any partial trailing bytes become the replacement or an
     *  error per the decoder's policy. */
    def decode(data: PyDynamic, finalChunk: Boolean): String = native

    /** Reset decoder state — drops any pending partial sequence. */
    def reset(): Unit = native

  def newUtf8Decoder(): Utf8IncrementalDecoder =
    new Utf8IncrementalDecoder()

  def lookupCanonicalNameOrNull(name: String): String | Null =
    runtime.codecLookup(name)

  def newDecoder(codec: String): IncrementalDecoder =
    val factory = codecs.getincrementaldecoder(codec).asInstanceOf[PyDynamic]
    new IncrementalDecoder(factory.`__call__`("strict").asInstanceOf[PyDynamic])

  def newEncoder(codec: String): IncrementalEncoder =
    val factory = codecs.getincrementalencoder(codec).asInstanceOf[PyDynamic]
    new IncrementalEncoder(factory.`__call__`("strict").asInstanceOf[PyDynamic])

  def encode(text: String, codec: String, errors: String): Array[Byte] =
    PyBytes.fromPyBytes(codecs.encode(text, codec, errors).asInstanceOf[PyDynamic])

  def decode(bytes: Array[Byte], codec: String, errors: String): String =
    codecs.decode(PyBytes.toPyBytes(bytes), codec, errors).asInstanceOf[String]

  def hasUnicodeErrorRange(error: Any): Boolean =
    error != null &&
      builtins.hasAttr(error, "start") &&
      builtins.hasAttr(error, "end")

  def unicodeErrorStart(error: Any): Int =
    if hasUnicodeErrorRange(error) then
      builtins.getAttr(error, "start").asInstanceOf[Int]
    else -1

  def unicodeErrorEnd(error: Any): Int =
    if hasUnicodeErrorRange(error) then
      builtins.getAttr(error, "end").asInstanceOf[Int]
    else -1

  def unicodeErrorReason(error: Any): String | Null =
    if error == null || !builtins.hasAttr(error, "reason") then null
    else builtins.getAttr(error, "reason").asInstanceOf[String | Null]

  def decoderBufferedInputLength(state: Any): Int =
    if state == null then 0
    else
      val buffered = operator.getItem(state, 0)
      builtins.lengthOf(buffered)

  private[runtime] def decodeStep(decoder: Any, bytes: Array[Byte], finalChunk: Boolean): DecodeStep =
    val raw = runtime.codecDecodeStep(decoder, PyBytes.toPyBytes(bytes), finalChunk)
    new DecodeStep(
      operator.getItem(raw, 0).asInstanceOf[String],
      operator.getItem(raw, 1).asInstanceOf[Int],
      operator.getItem(raw, 2).asInstanceOf[Int],
      operator.getItem(raw, 3).asInstanceOf[Int],
      operator.getItem(raw, 4).asInstanceOf[String | Null]
    )

  private[runtime] def encodeStep(encoder: Any, text: String, finalChunk: Boolean): EncodeStep =
    val raw = runtime.codecEncodeStep(encoder, text, finalChunk)
    new EncodeStep(
      PyBytes.fromPyBytes(operator.getItem(raw, 0).asInstanceOf[PyDynamic]),
      operator.getItem(raw, 1).asInstanceOf[Int],
      operator.getItem(raw, 2).asInstanceOf[Int],
      operator.getItem(raw, 3).asInstanceOf[Int],
      operator.getItem(raw, 4).asInstanceOf[String | Null]
    )

final class DecodeStep private[runtime] (
    val text: String,
    val consumed: Int,
    val errorStart: Int,
    val errorEnd: Int,
    val reason: String | Null
)

final class EncodeStep private[runtime] (
    val bytes: Array[Byte],
    val consumed: Int,
    val errorStart: Int,
    val errorEnd: Int,
    val reason: String | Null
)

final class IncrementalDecoder private[runtime] (private val underlying: PyDynamic):
  def decode(bytes: Array[Byte], finalChunk: Boolean): String =
    underlying.decode(PyBytes.toPyBytes(bytes), finalChunk).asInstanceOf[String]

  def decodeStep(bytes: Array[Byte], finalChunk: Boolean): DecodeStep =
    PyCodecs.decodeStep(underlying, bytes, finalChunk)

  def getState(): Any =
    underlying.getstate().asInstanceOf[Any]

  def setState(state: Any): Unit =
    underlying.setstate(state)

  def reset(): Unit =
    underlying.reset()

final class IncrementalEncoder private[runtime] (private val underlying: PyDynamic):
  def encode(text: String, finalChunk: Boolean): Array[Byte] =
    PyBytes.fromPyBytes(underlying.encode(text, finalChunk).asInstanceOf[PyDynamic])

  def encodeStep(text: String, finalChunk: Boolean): EncodeStep =
    PyCodecs.encodeStep(underlying, text, finalChunk)

  def getState(): Any =
    underlying.getstate().asInstanceOf[Any]

  def setState(state: Any): Unit =
    underlying.setstate(state)

  def reset(): Unit =
    underlying.reset()
