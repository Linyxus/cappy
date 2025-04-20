def testSuite[IO](a: IO^{cap}, b: IO^{cap}, c: IO^{cap}): Unit =
  val f1 = (x: IO^{a, b}) => x
  f1(a)
  f1(b)
  f1(c)
  ()
