// Method-scope `return` (and a `boundary.break`) escaping through a
// `try { ... } finally { ... }` block. The label escape is realized
// as `raise _scpy_lbl_<n>(value)` and the Python emitter must let the
// `finally` run before the exception continues propagating.
//
// Catches: a future regression where the emitter places the label
// escape *outside* a try-finally, or where the finally accidentally
// suppresses the label exception.

import scala.util.boundary, boundary.break

def returnThroughFinally(n: Int): Int =
  try
    if n == 0 then return 7
    n + 100
  finally
    println("finally-return")

def breakThroughFinally(n: Int): Int =
  boundary:
    try
      if n == 0 then break(11)
      n + 100
    finally
      println("finally-break")

@main def labeledTryFinallyReturn(): Unit =
  println("rtf(0):" + returnThroughFinally(0))
  println("rtf(5):" + returnThroughFinally(5))
  println("btf(0):" + breakThroughFinally(0))
  println("btf(5):" + breakThroughFinally(5))
