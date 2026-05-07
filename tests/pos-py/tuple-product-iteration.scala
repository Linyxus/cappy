@main def run(): Unit =
  val t = (10, "two", 3.0)
  println(t.productArity)
  println(t.productElement(0))
  println(t.productElement(1))
  println(t.productElement(2))
  println(t.productIterator.mkString("|"))
  println(t.productPrefix)
