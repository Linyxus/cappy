package java.nio.charset

private[charset] object UTF_16
    extends Charset("UTF-16", Array("utf16", "UTF_16", "UnicodeBig", "unicode")):

  def contains(cs: Charset): Boolean =
    this == cs

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "utf-16", 0.5f, 1.0f)

  def newEncoder(): CharsetEncoder =
    // JDK's `UTF-16` encoder emits a big-endian byte-order mark
    // (`FE FF`) followed by big-endian data. Python's `utf-16` codec
    // emits a platform-native (typically little-endian) BOM, which
    // diverges from JDK. Route encoding through `utf-16-be` and
    // prepend the BE BOM explicitly so ScalaPy output matches JDK
    // byte-for-byte.
    new PyCodecBackedEncoder(
      this,
      "utf-16-be",
      2.0f,
      4.0f,
      Array(-1.toByte, -3.toByte),
      Array(-2.toByte, -1.toByte)
    )
