import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CodingErrorAction, StandardCharsets}

/** Pins that the `Charset.decode(ByteBuffer)` / `Charset.encode(...)`
 *  convenience methods route through `newDecoder()` / `newEncoder()` —
 *  so every subclass hook and every default-replacement setting fires,
 *  matching JDK semantics. */
@main def javalibCharsetConvenience(): Unit =
  // Decode with an invalid UTF-8 byte: the convenience method must use
  // REPLACE on malformed input (JDK default) and emit U+FFFD.
  val malformed = ByteBuffer.wrap(Array[Byte](0x68.toByte, 0xC3.toByte, 0x69.toByte))
  val decoded = StandardCharsets.UTF_8.decode(malformed).toString()
  println("decode-replace:" + decoded.length() + ":" + (decoded.charAt(1).toInt == 0xFFFD))

  // Encode with an unmappable char through US-ASCII: REPLACE must use
  // the encoder's default replacement byte (`?` = 0x3F).
  val encoded = StandardCharsets.US_ASCII.encode(CharBuffer.wrap("h\u00E9i"))
  val asciiBytes = new Array[Byte](encoded.remaining())
  encoded.get(asciiBytes)
  println("encode-replace:" + asciiBytes.length + ":" +
    (asciiBytes(0) & 0xFF) + ":" +
    (asciiBytes(1) & 0xFF) + ":" +
    (asciiBytes(2) & 0xFF))

  // encode(String) delegates to encode(CharBuffer) — same REPLACE path.
  val fromString = StandardCharsets.US_ASCII.encode("h\u00E9i")
  val fromStringBytes = new Array[Byte](fromString.remaining())
  fromString.get(fromStringBytes)
  println("encode-string:" + (fromStringBytes(1) & 0xFF))

  // UTF-16 now emits big-endian BOM (FE FF) + BE data, matching JDK.
  val utf16 = StandardCharsets.UTF_16.encode("hi")
  val utf16Bytes = new Array[Byte](utf16.remaining())
  utf16.get(utf16Bytes)
  println("utf16-be-bom:" +
    (utf16Bytes(0) & 0xFF) + ":" +
    (utf16Bytes(1) & 0xFF) + ":" +
    (utf16Bytes(2) & 0xFF) + ":" +
    (utf16Bytes(3) & 0xFF) + ":" +
    (utf16Bytes(4) & 0xFF) + ":" +
    (utf16Bytes(5) & 0xFF))

  // Round-trip: JDK-shaped UTF-16 output must decode back through JDK
  // UTF-16 decoder (BOM-driven).
  val roundtrip = StandardCharsets.UTF_16.decode(ByteBuffer.wrap(utf16Bytes)).toString()
  println("utf16-roundtrip:" + roundtrip)
