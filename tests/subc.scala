def test[IO](a: IO^{cap}, b: IO^{cap}, c: IO^{cap}): Unit =
  val f = () =>
    a
    b
    ()
  val g = f
  val t1: () ->{f} Unit = g
  val t2: () ->{a, b} Unit = g
  val t3: () ->{a, b, a, a} Unit = g
