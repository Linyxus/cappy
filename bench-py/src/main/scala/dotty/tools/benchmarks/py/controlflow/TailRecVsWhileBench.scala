package dotty.tools.benchmarks.py.controlflow

import scala.annotation.tailrec

/** Same integer sum via hand-written `while`, `@tailrec` self-recursion (lowered
 *  to a loop), and a `for` comprehension over a `Range`. Contrasts the three
 *  loop-normalization shapes the backend produces. */
class TailRecVsWhileBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  @tailrec final def sumTail(i: Int, acc: Long): Long =
    if i <= 0 then acc else sumTail(i - 1, acc + i)

  val operations: Map[String, () => Any] = Map(
    "whileSum" -> { () =>
      var s = 0L
      var i = 0
      while i <= size do
        s += i
        i += 1
      s
    },
    "tailRecSum" -> (() => sumTail(size, 0L)),
    "forLoopSum" -> { () =>
      var s = 0L
      for i <- 0 to size do s += i
      s
    },
  )

@main def main(): Unit = ()
