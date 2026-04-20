import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CodingErrorAction, StandardCharsets, UnsupportedCharsetException}

@main def javalibCharsetCore(): Unit =
  println(
    "standard:" +
      StandardCharsets.UTF_8.name() +
      ":" +
      StandardCharsets.UTF_16.displayName() +
      ":" +
      StandardCharsets.UTF_8.aliases().contains("UTF8")
  )
  println(
    "forname:" +
      (Charset.forName("utf-8") eq StandardCharsets.UTF_8) +
      ":" +
      (Charset.forName("LaTin1") eq StandardCharsets.ISO_8859_1) +
      ":" +
      Charset.isSupported("cp1252") +
      ":" +
      Charset.isSupported("totally-fake")
  )

  val replaceDecoder = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPLACE)
  println("decoder-replace:" + replaceDecoder.decode(ByteBuffer.wrap(Array[Byte](0xC3.toByte))).toString())

  val reportDecoder = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
  val reportResult = reportDecoder.decode(ByteBuffer.wrap(Array[Byte](0xC3.toByte)), CharBuffer.allocate(8), true)
  println("decoder-report:" + reportResult.isMalformed() + ":" + reportResult.length())

  val reportEncoder = StandardCharsets.US_ASCII.newEncoder().onUnmappableCharacter(CodingErrorAction.REPORT)
  val encoderResult = reportEncoder.encode(CharBuffer.wrap("😀"), ByteBuffer.allocate(16), true)
  println("encoder-report:" + encoderResult.isUnmappable() + ":" + encoderResult.length())

  // Bogus names must surface as UnsupportedCharsetException at runtime,
  // never a compile-time diagnostic. `forName` and `isSupported` both
  // honour the contract.
  val forNameThrew =
    try
      Charset.forName("totally-fake")
      false
    catch
      case _: UnsupportedCharsetException => true
  println("forname-throws:" + forNameThrew)
