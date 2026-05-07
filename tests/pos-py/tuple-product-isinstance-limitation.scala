// Documents a known limitation of the tuple-as-Python-tuple lowering.
//
// On the JVM, `Tuple{N}` extends `Product{N}` extends `Product`, so a
// Scala tuple satisfies `isInstanceOf[Product]` and matches the
// `case _: Product` extractor. After the Python-backend lowering,
// Scala tuples are bare `tuple` subclass instances — they no longer
// inherit from a Python `Product` class, so reflective type checks
// against `Product` itself fail.
//
// METHOD CALLS through a `Product`-typed receiver still work via the
// `Phase B1` polyfill (intercepted at codegen, regardless of static
// receiver type), so user code that does
//
//     def report(p: Product): Int = p.productArity
//     report(t)
//
// is fine. What is NOT fine, and is exercised below, is:
//
//   - `t.isInstanceOf[Product]`
//   - `case p: Product => …` (the type test fails, the arm is skipped)
//
// This is intentional given the implementation strategy: tagging the
// runtime tuple as a `Product` would require either (a) a dedicated
// `_scpy_ScalaTuple` registered as deriving from a stub `Product`
// Python class, or (b) registering each `Tuple{N}` Python type as a
// real subclass of a `Product` stub. Both options are out of scope
// for the static-Tuple-call-site lowering.
//
// This test pins the limited behaviour so any change that flips it
// becomes a deliberate, reviewed step rather than silent drift.

@main def tupleProductIsInstanceLimitation(): Unit =
  val t: (Int, String, Boolean) = (1, "x", true)

  // Direct reflective check. JVM: true. Python backend: false.
  println(t.isInstanceOf[Product])

  // Pattern match through `Any`. JVM: matches the `Product` arm.
  // Python backend: falls through to the default arm.
  val any: Any = t
  any match
    case _: Product => println("matched Product")
    case _          => println("did not match Product")

  // The METHOD-CALL polyfill still works — calling `productArity`
  // through a `Product`-typed receiver is fine, even though the
  // `isInstanceOf` check that would normally precede such a call
  // is broken. Stated bluntly: you cannot DETECT a tuple as a
  // Product, but if you already KNOW it is one, you can still
  // call Product methods on it.
  def report(p: Product): String =
    s"arity=${p.productArity}, prefix=${p.productPrefix}"
  println(report(t))

  // The narrower `Tuple{N}` checks DO succeed, because
  // `_scpy_is_value_of_type` resolves them by inspecting `len(value)`
  // rather than walking a Python class hierarchy.
  println(t.isInstanceOf[Tuple3[?, ?, ?]])
  any match
    case _: Tuple3[?, ?, ?] => println("matched Tuple3")
    case _                  => println("did not match Tuple3")
