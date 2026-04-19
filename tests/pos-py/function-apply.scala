@main def functionApply(): Unit =
  // Real Function0, allocated + stored + invoked.
  val thunk: () => Int = () => 42
  println("direct:" + thunk())

  // Pass through a non-inline def — forces real Function0 allocation.
  def run(f: () => Int): Int = f()
  println("thru:" + run(() => 7))

  // Capture test.
  var counter = 0
  val inc: () => Unit = () => counter = counter + 1
  inc()
  inc()
  inc()
  println("capture:" + counter)

  // Function1.
  val inc1: Int => Int = x => x + 1
  println("fn1:" + inc1(41))

  // Function2.
  val add: (Int, Int) => Int = (a, b) => a + b
  println("fn2:" + add(3, 4))

  // Passing lambda to helper that stores and invokes later.
  def catchAny(thunk: () => Any): String =
    try
      thunk()
      "no-throw"
    catch
      case _: ArithmeticException => "caught-arith"
  println("catch-happy:" + catchAny(() => 42))
  println("catch-arith:" + catchAny(() => throw new ArithmeticException("boom")))
