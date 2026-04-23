case class Point(x: Int, y: Int)
case class Marker()
case class Labeled(point: Point, label: String = "origin")

@main def run(): Unit =
  val p = Point(10, 10)
  println("The point is: " + p)
  println(p == Point(10, 10))
  println(p.copy(y = 20))

  val labeled = Labeled(p)
  println(labeled)
  println(labeled.copy(point = Point(1, 2), label = "moved"))
  println(labeled.productPrefix)
  println(labeled.productElementName(0))
  println(labeled.productElementName(1))

  val m = Marker()
  println(m)
  println(m == Marker())
