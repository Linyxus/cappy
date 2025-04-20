def testSuite[IO](a: IO^{cap}, b: IO^{cap}, c: IO^{cap}): Unit =
  val f1 = (x: () ->{cap} IO^{cap}, y: IO^{x}) => x
  val f2 = (x: IO^{cap}) => () => x
  val t3 = f2(a)
  val t4 = f2(b)
  ()
