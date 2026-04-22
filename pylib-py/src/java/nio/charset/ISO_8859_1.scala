package java.nio.charset

private[charset] object ISO_8859_1
    extends Charset(
      "ISO-8859-1",
      Array(
        "csISOLatin1",
        "IBM-819",
        "iso-ir-100",
        "8859_1",
        "ISO_8859-1",
        "l1",
        "ISO8859-1",
        "ISO_8859_1",
        "cp819",
        "ISO8859_1",
        "latin1",
        "ISO_8859-1:1987",
        "819",
        "IBM819"
      )
    ):

  def contains(cs: Charset): Boolean =
    (this == cs) || (cs == US_ASCII)

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, "latin-1", 1.0f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, "latin-1", 1.0f, 1.0f)
