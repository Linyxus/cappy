@main def run(): Unit =
  val a = (1, 2)
  val b = (3, 4, 5)
  val c = a ++ b
  println(c)
  println(c.size)

  val left: (Int, String) = (1, "x")
  val right: (Boolean, Double) = (true, 1.5)
  val d = left ++ right
  println(d)
  println(d._1)
  println(d._2)
  println(d._3)
  println(d._4)

  val t1 = (1, 2)
  val t2 = (3, 4)
  val t3 = (5, 6)
  println((t1 ++ t2) ++ t3 == t1 ++ (t2 ++ t3))
