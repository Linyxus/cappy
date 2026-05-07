@main def run(): Unit =
  val t1 = Tuple1(99)
  println(t1.head)
  println(t1.last)

  val t2: (Int, Int) = (10, 20)
  println(t2.head)
  println(t2.last)

  val t5 = ("a", 1, true, 3.14, "end")
  println(t5.head)
  println(t5.last)
