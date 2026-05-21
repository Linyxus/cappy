package dotty.tools.benchmarks.py.recursion

import scala.annotation.tailrec

/** `@tailrec` self-recursion: should lower to a loop in the backend, so this
 *  measures how close tail-recursive code gets to a hand-written `while`. */
class TailRecBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  @tailrec final def countDown(i: Int, acc: Int): Int =
    if i <= 0 then acc else countDown(i - 1, acc + 1)

  @tailrec final def sumTail(i: Int, acc: Long): Long =
    if i <= 0 then acc else sumTail(i - 1, acc + i)

  val operations: Map[String, () => Any] = Map(
    "countDown" -> (() => countDown(size, 0)),
    "sumTail"   -> (() => sumTail(size, 0L)),
  )

@main def main(): Unit = ()
