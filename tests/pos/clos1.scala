def test[IO](a: IO^{cap}, b: IO^{cap}): Unit =
  val t1: () ->{a} IO^{a} = () => a
  val t2: () -> () ->{a} IO^{a} = () => () => a
  val t3: () -> () ->{b} () ->{a} IO^{a} = () => () => 
    b
    () => a
  ()