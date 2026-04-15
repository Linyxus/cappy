import scala.python.*

// Mixed positional + keyword argument lowering. Scala's desugaring for
// `d.foo(a, b, k = v)` produces `applyDynamicNamed("foo")(("", a), ("", b), ("k", v))`
// — positional args get empty-string names as the sentinel. The backend
// partitions by empty-name and puts positional values into
// `PyApplyDynamic.args`, keyword pairs into `PyApplyDynamic.kwargs`.

@main def kwargsMixed(): Unit =
  val b = Dynamic.module("builtins")
  // Three positional + sep/end kwargs.
  b.print("a", "b", "c", sep = "|", end = "!\n")
  // Single positional + kwarg.
  b.print("one", end = " ")
  b.print("two", end = "\n")
  // Mixed with non-string positional values.
  b.print("x=", 1, "y=", 2.5, sep = "", end = "\n")
