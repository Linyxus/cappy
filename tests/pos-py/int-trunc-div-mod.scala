// Verify that JVM-style truncate-toward-zero `/` and `%` survive
// the Python lowering. Python's native `//` and `%` are floor-style
// (sign of divisor, not dividend), so a runtime helper handles the
// negative-operand cases. See `_scpy_int_trunc_div` /
// `_scpy_int_trunc_mod` in PyIRRuntime.scala.

@main def intTruncDivMod(): Unit =
  println("7/3=" + (7 / 3))
  println("-7/3=" + (-7 / 3))
  println("7/-3=" + (7 / -3))
  println("-7/-3=" + (-7 / -3))

  println("7%3=" + (7 % 3))
  println("-7%3=" + (-7 % 3))
  println("7%-3=" + (7 % -3))
  println("-7%-3=" + (-7 % -3))

  // Long versions exercise the same helper at 64-bit width.
  val a: Long = 7000000000L
  val b: Long = 3L
  println("L 7e9/3=" + (a / b))
  println("L -7e9/3=" + (-a / b))
  println("L 7e9%3=" + (a % b))
  println("L -7e9%3=" + (-a % b))
