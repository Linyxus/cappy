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
  captureStringDeferred("format") {
    java.lang.String.format("%s", Array[AnyRef]("x"))
  }
