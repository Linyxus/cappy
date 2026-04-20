import java.nio.charset.{Charset, StandardCharsets}

private def bytesOf(xs: Array[Byte]): String =
  val sb = new java.lang.StringBuilder()
  var i = 0
  while i < xs.length do
    if i != 0 then sb.append(',')
    sb.append(xs(i) & 0xff)
    i += 1
  sb.toString()

@main def javalibLangStringCharset(): Unit =
  val latin1 = "héllo".getBytes("ISO-8859-1")
  println("latin1-name:" + bytesOf(latin1) + ":" + new String(latin1, "ISO-8859-1"))

  val charset = Charset.forName("ISO-8859-1")
  val latin1ViaCharset = "héllo".getBytes(charset)
  println("latin1-charset:" + bytesOf(latin1ViaCharset) + ":" + new String(latin1ViaCharset, charset))

  val windows = "héllo €".getBytes("windows-1252")
  println("windows1252:" + new String(windows, "cp1252") + ":" + Charset.forName("cp1252").name())

  val utf16 = "ScalaPy".getBytes(StandardCharsets.UTF_16)
  println("utf16-charset:" + bytesOf(utf16) + ":" + new String(utf16, StandardCharsets.UTF_16))
