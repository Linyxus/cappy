private def captureStringDeferred(tag: String)(body: => Any): Unit =
  try
    body
    println(tag + ":no-throw")
  catch
    case e: UnsupportedOperationException =>
      println(tag + ":" + e.getMessage())

@main def javalibLangStringDeferred(): Unit =
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
