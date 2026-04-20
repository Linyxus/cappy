package java.nio.charset

private[charset] object US_ASCII
    extends Charset(
      "US-ASCII",
      Array(
        "cp367",
        "ascii7",
        "ISO646-US",
        "646",
        "csASCII",
        "us",
        "iso_646.irv:1983",
        "ISO_646.irv:1991",
        "IBM367",
        "ASCII",
        "default",
        "ANSI_X3.4-1986",
        "ANSI_X3.4-1968",
        "iso-ir-6"
      )
    ):

  def contains(cs: Charset): Boolean =
    this == cs

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "ascii", 1.0f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, "ascii", 1.0f, 1.0f)
