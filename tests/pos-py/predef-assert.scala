@main def predefAssert(): Unit =
  assert(true)
  require(1 + 1 == 2)
  val x = identity(42)
  println(x)
  println(locally { "hello" })
