@main def run(): Unit =
  val a: (Int, String, Boolean) = (1, "x", true)
  val b: Int *: String *: Boolean *: EmptyTuple = 1 *: "x" *: true *: EmptyTuple
  println(a == b)
  println(a.hashCode == b.hashCode)
  println(a)
  println(b)
  println(a._1)
  println(a._2)
  println(a._3)
