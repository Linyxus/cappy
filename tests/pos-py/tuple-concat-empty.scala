@main def run(): Unit =
  val t = (1, "x", true)
  val a = EmptyTuple ++ t
  val b = t ++ EmptyTuple
  println(a == t)
  println(b == t)
  println(a)
  println(b)
  println((EmptyTuple ++ EmptyTuple) == EmptyTuple)
