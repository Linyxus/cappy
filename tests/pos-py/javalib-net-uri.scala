import java.net.URI

@main def javalibNetUri(): Unit =
  val parsed = new URI("https://user:pass@example.com:8042/a/b?x=1#frag")
  println(
    "parse:" +
      parsed.getScheme() + ":" +
      parsed.getUserInfo() + ":" +
      parsed.getHost() + ":" +
      parsed.getPort() + ":" +
      parsed.getPath() + ":" +
      parsed.getQuery() + ":" +
      parsed.getFragment()
  )
  println("authority:" + parsed.getAuthority() + ":" + parsed.getRawAuthority())

  val fromParts = new URI("https", "user name", "example.com", 8042, "/a b", "q=1 2", "frag ment")
  println("parts:" + fromParts.toString())

  val base = new URI("https://example.com/a/b/c")
  println("resolve:" + base.resolve("../d?x=1#f"))

  val relativizedBase = new URI("https://example.com/a/b/")
  val relativizedTarget = new URI("https://example.com/a/b/c/d")
  println("relativize:" + relativizedBase.relativize(relativizedTarget))

  val normalized = new URI("https://example.com/a/./b/../c/")
  println("normalize:" + normalized.normalize())

  val ascii = new URI("https://example.com/Größe")
  println("toascii:" + ascii.toASCIIString())

  try
    new URI("https://example.com/bad path")
    println("bad-space:fail")
  catch
    case e: java.net.URISyntaxException =>
      println("bad-space:" + e.getReason())

  try
    new URI("https://example.com/%zz")
    println("bad-escape:fail")
  catch
    case e: java.net.URISyntaxException =>
      println("bad-escape:" + e.getReason() + ":" + e.getIndex())
