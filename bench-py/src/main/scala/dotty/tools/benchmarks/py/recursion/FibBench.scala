package dotty.tools.benchmarks.py.recursion

/** Method-call recursion vs an iterative loop, both scaled by `size`.
 *  `recursive` repeats a bounded-depth naive `fib` to stress the
 *  call/return path; `iterative` is the same Fibonacci recurrence as a
 *  straight-line `while` loop. */
class FibBench:
  /** Fixed recursion depth per call — small enough that one `fib(depth)`
   *  stays cheap, so total work scales linearly with `size`. */
  private final val depth = 15

  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  def fib(k: Int): Long =
    if k < 2 then k.toLong else fib(k - 1) + fib(k - 2)

  val operations: Map[String, () => Any] = Map(
    "recursive" -> { () =>
      var acc = 0L
      var i = 0
      while i < size do
        acc += fib(depth)
        i += 1
      acc
    },
    "iterative" -> { () =>
      var a = 0L
      var b = 1L
      var i = 0
      while i < size do
        val t = a + b
        a = b
        b = t
        i += 1
      a
    },
  )

@main def main(): Unit = ()
