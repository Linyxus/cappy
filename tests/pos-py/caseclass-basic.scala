object CaseclassBasic:
  case class Point(x: Int, y: Int)
  case class Empty()
  case class Config(host: String = "localhost", port: Int = 80)
  case class Inner(label: String)
  case class Outer(inner: Inner, count: String)

@main def run(): Unit =
  val p = CaseclassBasic.Point(10, 20)
  val p2 = CaseclassBasic.Point(10, 20)
  val p3 = CaseclassBasic.Point(20, 10)
  println(p)
  println(p == p2)
  println(p == p3)
  println(p.copy(y = 99))
  println(p.productArity)
  println(p.productElement(0))
  println(p.productElement(1))
  println(p.productElementName(0))
  println(p.productElementName(1))
  println(p.productIterator.mkString("|"))
  println(p.canEqual(p2))

  val e1 = CaseclassBasic.Empty()
  val e2 = CaseclassBasic.Empty()
  println(e1)
  println(e1 == e2)
  println(e1.copy())
  println(e1.productPrefix)
  println(e1.productArity)

  println(CaseclassBasic.Config())
  println(CaseclassBasic.Config(port = 8080))
  println(CaseclassBasic.Config("remote").copy(port = 9000))

  val outer = CaseclassBasic.Outer(CaseclassBasic.Inner("x"), "2")
  println(outer)
  println(outer == CaseclassBasic.Outer(CaseclassBasic.Inner("x"), "2"))
  println(outer == CaseclassBasic.Outer(CaseclassBasic.Inner("y"), "2"))
  println(outer.copy(inner = CaseclassBasic.Inner("z")))

  try
    println(p.productElement(2))
  catch
    case e: IndexOutOfBoundsException => println("oob:" + e.getMessage())
