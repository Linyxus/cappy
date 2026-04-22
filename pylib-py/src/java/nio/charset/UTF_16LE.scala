package java.nio.charset

private[charset] object UTF_16LE
    extends Charset(
      "UTF-16LE",
      Array("UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE")
    ):

  def contains(cs: Charset): Boolean =
    this == cs

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "utf-16-le", 0.5f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, "utf-16-le", 2.0f, 2.0f, Array(-3.toByte, -1.toByte))
