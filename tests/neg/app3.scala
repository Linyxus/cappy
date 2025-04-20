def testSuite[IO](a: IO^{cap}, b: IO^{cap}, c: IO^{cap}): Unit =
  val f1 = (x: IO^{cap}, y: IO^{x}) => x
  f1(a, a)
  f1(a, b)
  ()
