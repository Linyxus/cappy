package java.nio.charset

private[charset] object UTF_16BE
    extends Charset(
      "UTF-16BE",
      Array("X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2", "UnicodeBigUnmarked")
    ):

  def contains(cs: Charset): Boolean =
    this == cs

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "utf-16-be", 0.5f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, "utf-16-be", 2.0f, 2.0f, Array(-1.toByte, -3.toByte))
