def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  val f1 = [cap C, X <: IO^{C}] => 0
  ()
