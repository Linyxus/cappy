// A method-scope `return` and a `boundary.break` must NOT be caught
// by `catch case _: Throwable`. The label escape lowers to a raise
// of `_scpy_lbl_<n>`, a class that is NOT a subtype of the runtime
// `Throwable` Python wrapper, so user code's broad catch handlers
// stay out of its way.
//
// Catches: any regression where `_scpy_lbl_<n>` becomes a Throwable
// subclass (Python BaseException base would also do for a strict
// Throwable check, but `except BaseException` is rarely written in
// user code, while `case _: Throwable` is — and is what would
// silently break).

import scala.util.boundary, boundary.break

def passThroughCatch(n: Int): Int =
  try
    if n == 0 then return 21
    n + 100
  catch
    case _: Throwable =>
      println("caught-return")
      -1

def passThroughBreak(n: Int): Int =
  boundary:
    try
      if n == 0 then break(31)
      n + 100
    catch
      case _: Throwable =>
        println("caught-break")
        -1

@main def labeledCatchThrowablePassthrough(): Unit =
  println("ptc(0):" + passThroughCatch(0))
  println("ptc(5):" + passThroughCatch(5))
  println("ptb(0):" + passThroughBreak(0))
  println("ptb(5):" + passThroughBreak(5))
