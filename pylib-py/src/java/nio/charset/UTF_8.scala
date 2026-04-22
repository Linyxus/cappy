package java.nio.charset

private[charset] object UTF_8
    extends Charset("UTF-8", Array("UTF8", "unicode-1-1-utf-8")):

  def contains(cs: Charset): Boolean =
    true

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "utf-8", 1.0f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, "utf-8", 1.1f, 3.0f)
