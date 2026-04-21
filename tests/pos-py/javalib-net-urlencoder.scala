import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets

@main def javalibNetUrlencoder(): Unit =
  val sample = "hello world/~*"
  val encodedUtf8 = URLEncoder.encode(sample, StandardCharsets.UTF_8)
  println("encode-utf8:" + encodedUtf8)
  println("decode-utf8:" + URLDecoder.decode(encodedUtf8, StandardCharsets.UTF_8))

  val unicode = "Größe Zürich"
  val encodedUnicode = URLEncoder.encode(unicode, "UTF-8")
  println("encode-unicode:" + encodedUnicode)
  println("decode-unicode:" + URLDecoder.decode(encodedUnicode, "UTF-8"))

  try
    URLEncoder.encode("x", "nope-charset")
    println("bad-charset:fail")
  catch
    case e: java.io.UnsupportedEncodingException =>
      println("bad-charset:" + e.getMessage())
