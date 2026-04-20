import java.util.Base64

@main def javalibUtilBase64(): Unit =
  val encoded = Base64.getEncoder().encodeToString("hello".getBytes("UTF-8"))
  val decoded = new String(Base64.getDecoder().decode(encoded), "UTF-8")
  val url = Base64.getUrlEncoder().withoutPadding().encodeToString(Array[Byte](1, 2, 3))
  println("basic:" + encoded + ":" + decoded)
  println("url:" + url)
