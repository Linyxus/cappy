def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  def x = a
  def y =
    b
    1
  val f: () -> i64 = () => y
