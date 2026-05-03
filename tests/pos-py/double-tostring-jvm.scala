// Verifies that `Double.toString` (and the implicit boxing path
// `println(Double)` -> `String.valueOf(Object)` -> `Object.toString`)
// produces JVM-shaped formatting on top of Python's `repr(float)`:
// scientific notation thresholds at 1e-3 / 1e7, capital `E`, no `+`
// sign on positive exponents, no zero-padding, trailing `.0` for
// integer-valued mantissas. See Wave 5 item 12 — FP-rounding bucket.

@main def doubleToStringJvm(): Unit =
  // Plain notation: 0.001 <= |x| < 1e7 stays decimal, with .0 appended.
  println(java.lang.Double.toString(1.0))
  println(java.lang.Double.toString(0.001))
  println(java.lang.Double.toString(0.5))
  println(java.lang.Double.toString(1234567.0))
  // Edge: 0.0001 — Java picks scientific, Python's repr picks plain.
  println(java.lang.Double.toString(0.0001))

  // Scientific notation: |x| < 1e-3 OR |x| >= 1e7.
  println(java.lang.Double.toString(1.0e-9))
  println(java.lang.Double.toString(1.5e-9))
  println(java.lang.Double.toString(1.0e20))
  println(java.lang.Double.toString(2.5e7))

  // Sign + zero forms.
  println(java.lang.Double.toString(-1.0))
  println(java.lang.Double.toString(0.0))
  println(java.lang.Double.toString(-0.0))

  // String concat path: same value should print identically.
  println("" + 1.5e-9)
  println("" + 1.0e20)

  // Boxed Object path: `println(Any)` boxes a Double; the runtime
  // helper must reach `Double.toString`.
  val boxed: Any = 1.5e-9
  println(boxed)
