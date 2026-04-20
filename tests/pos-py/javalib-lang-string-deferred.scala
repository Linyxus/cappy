private def captureStringDeferred(tag: String)(body: => Any): Unit =
  try
    body
    println(tag + ":no-throw")
  catch
    case e: UnsupportedOperationException =>
      println(tag + ":" + e.getMessage())

@main def javalibLangStringDeferred(): Unit =
  captureStringDeferred("matches") {
    "abc".matches("a.*")
  }
  captureStringDeferred("split1") {
    "a,b".split(",")
  }
  captureStringDeferred("split2") {
    "a,b".split(",", 2)
  }
  captureStringDeferred("replaceAll") {
    "aba".replaceAll("a", "x")
  }
  captureStringDeferred("replaceFirst") {
    "aba".replaceFirst("a", "x")
  }
  println("format:" + java.lang.String.format("%s", Array[AnyRef]("x")))
  // Backend-intercepted Locale overloads — the backend emits
  // `_scpy_unsupported` before the null receiver is touched.
  val locale = null.asInstanceOf[java.util.Locale]
  captureStringDeferred("lower-locale") {
    "abc".toLowerCase(locale)
  }
  captureStringDeferred("upper-locale") {
    "abc".toUpperCase(locale)
  }
  // getBytes(Charset) — runtime-side unsupported check in
  // `_scpy_str_get_bytes` (non-string encoding argument).
  val charset = null.asInstanceOf[java.nio.charset.Charset]
  captureStringDeferred("getbytes-charset") {
    "abc".getBytes(charset)
  }
  // charset-name paths that aren't in the UTF-8 / US-ASCII / ISO-8859-1
  // allowlist — the companion raises UnsupportedOperationException.
  captureStringDeferred("new-charset-name") {
    new String(Array[scala.Byte](65.toByte), "Windows-1252")
  }
