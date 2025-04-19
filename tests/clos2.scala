def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  val t1 = () => () => a
  val t2 = [X] => () => b
  val t3 = [cap C] => () => a
  val t4 = [cap C1] => [cap C2] =>
    a
    b
    ()
  val t5 = [X] =>
    a
    () => 
      b
  ()

