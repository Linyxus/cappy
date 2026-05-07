@main def run(): Unit =
  val a = (1, 2, 3)
  val b = ("x", "y", "z")
  println(a.zip(b))

  val left = (1, 2, 3, 4)
  val short = ("a", "b")
  println(left.zip(short))

  val right = (1, 2)
  val long = ("p", "q", "r", "s")
  println(right.zip(long))

  println(EmptyTuple.zip((1, 2, 3)))
  println((1, 2, 3).zip(EmptyTuple))
