import scala.python.*

// Basic keyword-argument lowering. Scala `d.foo(k = v)` desugars to
// `d.applyDynamicNamed("foo")(("k", v))` which the backend lowers to
// `PyApplyDynamic(callee, args=[], kwargs=[("k", v)])`. The emitter
// renders this as Python named-call syntax `callee(k=v)`.

@main def kwargsBasic(): Unit =
  val b = Dynamic.module("builtins")
  // Pure keyword args: `print(end="!!\n")` writes nothing followed by "!!".
  b.print(end = "!!\n")
  // Two keyword args in one call.
  b.print("x", end = " ", flush = true)
  b.print("y")
