@main def run(): Unit =
  val t = (1, 2, 3, 4, 5)
  println(t.take(0))
  println(t.take(2))
  println(t.take(5))
  println(t.take(100))
  println(t.drop(0))
  println(t.drop(3))
  println(t.drop(5))
  println(t.drop(100))

  val (a, b) = t.splitAt(2)
  println(a)
  println(b)
  println((a ++ b) == t)

  val (e, full) = t.splitAt(0)
  println(e)
  println(full)

  val (full2, e2) = t.splitAt(5)
  println(full2)
  println(e2)
