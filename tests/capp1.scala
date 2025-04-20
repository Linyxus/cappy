def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  val f1 = [cap C] => (op: () ->{C} Unit) => op()
  val f2 = () =>
    a
    ()
  val t3 = f1[{a}](f2)
  val f4 = () =>
    b
    ()
  val t5 = f1[{a, b}](f4)
