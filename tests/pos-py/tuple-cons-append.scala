@main def run(): Unit =
  val t1 = 1 *: EmptyTuple
  println(t1)
  println(t1._1)

  val t4 = 0 *: (1, 2, 3)
  println(t4)
  println(t4.size)

  val mixed = 1 *: "two" *: 3.0 *: EmptyTuple
  println(mixed)

  val a1 = EmptyTuple :* 1
  println(a1)

  val a3 = (1, 2) :* 3
  println(a3)
  println(a3._3)
