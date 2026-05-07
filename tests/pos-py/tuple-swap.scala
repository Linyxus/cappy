@main def run(): Unit =
  val t: (Int, String) = (42, "hello")
  val s: (String, Int) = t.swap
  println(s._1)
  println(s._2)
  println(t.swap.swap == t)
  println(t.swap.swap)
