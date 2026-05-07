@main def run(): Unit =
  val t1 = Tuple1(99)
  println(t1.tail == EmptyTuple)
  println(t1.init == EmptyTuple)

  val t3 = (1, "two", 3.0)
  println(t3.tail)
  println(t3.init)

  val t6 = (1, 2, 3, 4, 5, 6)
  println(t6.tail)
  println(t6.init)
