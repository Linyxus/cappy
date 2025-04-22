def test[IO](a: IO^{cap}): Unit =
  val f = () => () => a
  val g = () => f()
  val h = () => f()()
