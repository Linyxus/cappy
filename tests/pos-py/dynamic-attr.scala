import scala.python.*

@main def dynamicAttr(): Unit =
  // Literal single-segment path: lowers to a direct import from builtins.
  val printRef = Dynamic.attr("print")
  // Call via applyDynamic (scala.Dynamic rewrites `x.foo(args)` only when
  // there is a selection step - we invoke by requesting the `__call__`
  // attribute, which is always present on callable Python values).
  printRef.applyDynamic("__call__")("hello from Dynamic.attr")

  // Literal multi-segment path: walks `builtins.float.__name__` at emission.
  val name = Dynamic.attr("float.__name__")
  printRef.applyDynamic("__call__")("float name is", name)
