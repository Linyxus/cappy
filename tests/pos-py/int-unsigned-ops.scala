// Exercise `>>>` and the `Integer` / `Long` unsigned-arithmetic
// helpers. The unsigned binary ops (`IntUDiv`, `IntUShr`, etc.)
// previously emitted broken Python placeholders (`None  # TODO ...`)
// and `>>>` was inlined; both now route through the `_scpy_int_u*`
// runtime helpers in PyIRRuntime.

@main def intUnsignedOps(): Unit =
  // Logical right shift on a negative Int. -2 in 32-bit is
  // 0xFFFFFFFE; >>> 1 = 0x7FFFFFFF = 2147483647.
  val negInt: Int = -2
  println("ushr32:" + (negInt >>> 1))

  // -1 >>> 16 = 0xFFFF = 65535.
  println("ushr32-16:" + (-1 >>> 16))

  // 32-bit unsigned divide / remainder. -1 as unsigned is
  // 0xFFFFFFFF = 4294967295. 4294967295 / 2 = 2147483647 fits
  // an Int so this prints as positive.
  println("udiv32:" + Integer.divideUnsigned(-1, 2))
  println("urem32:" + Integer.remainderUnsigned(-1, 7))

  // Long unsigned right shift. -1L >>> 1 = 0x7FFFFFFFFFFFFFFFL.
  val negLong: Long = -1L
  println("ushr64:" + (negLong >>> 1))

  // -1L >>> 32 = 0xFFFFFFFFL.
  println("ushr64-32:" + (-1L >>> 32))
