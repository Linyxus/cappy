package java.util

import scala.python.runtime.PyBase64

/** `java.util.Base64` facaded over Python's `base64` module (RFC 4648
 *  basic + URL-safe). The previous hand-rolled bit-fiddling port has
 *  been retired — Python's implementation is C-level fast and tracks
 *  the RFC; we're a thin adapter.
 *
 *  MIME variant currently aliases to basic (no line-wrapping), which
 *  matches what downstream tests assert. If a downstream consumer
 *  needs the 76-char line break behaviour, extend `PyBase64` with a
 *  chunked encoder. */
object Base64 {
  def getEncoder(): Encoder = new Encoder(urlSafe = false, padding = true)
  def getUrlEncoder(): Encoder = new Encoder(urlSafe = true, padding = true)
  def getMimeEncoder(): Encoder = new Encoder(urlSafe = false, padding = true)
  def getMimeEncoder(lineLength: Int, lineSeparator: Array[Byte]): Encoder =
    // Line-wrapping not yet implemented; callers that rely on 76-char
    // chunks should switch to PyBase64 directly until a chunked
    // encoder lands.
    new Encoder(urlSafe = false, padding = true)

  def getDecoder(): Decoder = new Decoder(urlSafe = false)
  def getUrlDecoder(): Decoder = new Decoder(urlSafe = true)
  def getMimeDecoder(): Decoder = new Decoder(urlSafe = false)

  final class Encoder private[Base64] (urlSafe: Boolean, padding: Boolean) {
    def encode(src: Array[Byte]): Array[Byte] =
      if urlSafe then PyBase64.encodeUrlSafe(src, padding)
      else PyBase64.encodeBasic(src, padding)

    def encodeToString(src: Array[Byte]): String =
      new String(encode(src), "ISO-8859-1")

    def withoutPadding(): Encoder =
      new Encoder(urlSafe, padding = false)
  }

  final class Decoder private[Base64] (urlSafe: Boolean) {
    def decode(src: Array[Byte]): Array[Byte] =
      try
        if urlSafe then PyBase64.decodeUrlSafe(src)
        else PyBase64.decodeBasic(src)
      catch
        case t: Throwable =>
          throw new IllegalArgumentException("Illegal base64 input", t)

    def decode(src: String): Array[Byte] =
      decode(src.getBytes("ISO-8859-1"))
  }
}
