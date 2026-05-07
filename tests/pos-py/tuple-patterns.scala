@main def run(): Unit =
  val t = (1, "two", true)

  t match
    case h *: tail =>
      println(h)
      println(tail)

  t match
    case a *: b *: c *: EmptyTuple =>
      println(a)
      println(b)
      println(c)

  val r1 = (10, 20, 30) match
    case Tuple3(a, b, c) => a + b + c
  println(r1)

  val r2 = (5, 7, 11) match
    case (x, y, z) => s"$x:$y:$z"
  println(r2)

  EmptyTuple match
    case EmptyTuple => println("empty")
