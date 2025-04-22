def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  def f1() = a
  ()
