import java.nio.charset.StandardCharsets

private def bytesOf(xs: Array[Byte]): String =
  val sb = new java.lang.StringBuilder()
  var i = 0
  while i < xs.length do
    if i != 0 then sb.append(',')
    sb.append(xs(i) & 0xff)
    i += 1
  sb.toString()

@main def javalibCharsetRoundtrips(): Unit =
  val iso = "hié".getBytes(StandardCharsets.ISO_8859_1)
  println("iso88591:" + bytesOf(iso) + ":" + new String(iso, StandardCharsets.ISO_8859_1))

  val ascii = "hi".getBytes(StandardCharsets.US_ASCII)
  println("usascii:" + bytesOf(ascii) + ":" + new String(ascii, StandardCharsets.US_ASCII))
  val asciiErr = StandardCharsets.US_ASCII.newEncoder()
    .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
    .encode(java.nio.CharBuffer.wrap("é"), java.nio.ByteBuffer.allocate(8), true)
  println("usascii-unmappable:" + asciiErr.isUnmappable() + ":" + asciiErr.length())

  val utf8 = "hié".getBytes(StandardCharsets.UTF_8)
  println("utf8:" + bytesOf(utf8) + ":" + new String(utf8, StandardCharsets.UTF_8))

  val utf16 = "hié".getBytes(StandardCharsets.UTF_16)
  println("utf16:" + bytesOf(utf16) + ":" + new String(utf16, StandardCharsets.UTF_16))

  val utf16be = "hié".getBytes(StandardCharsets.UTF_16BE)
  println("utf16be:" + bytesOf(utf16be) + ":" + new String(utf16be, StandardCharsets.UTF_16BE))

  val utf16le = "hié".getBytes(StandardCharsets.UTF_16LE)
  println("utf16le:" + bytesOf(utf16le) + ":" + new String(utf16le, StandardCharsets.UTF_16LE))
