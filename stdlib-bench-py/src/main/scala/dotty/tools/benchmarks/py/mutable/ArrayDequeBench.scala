package dotty.tools.benchmarks.py.mutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.mutable.ArrayDeque
import scala.compiletime.uninitialized

class ArrayDequeBench extends BenchmarkSuite:
  var size:  Int = 0
  var deque: ArrayDeque[Int] = uninitialized
  var half:  Int = 0

  def setup(size: Int): Unit =
    this.size = size
    deque = new ArrayDeque[Int](size)
    var i = 0
    while i < size do
      deque += i
      i += 1
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val d = new ArrayDeque[Int](size)
      var i = 0
      while i < size do
        d += i
        i += 1
      d
    },
    "access"    -> (() => deque(half)),
    "transform" -> (() => deque.map(_ + 1)),
    "mutate"    -> { () => deque(half) = deque(half) },
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new ArrayDequeBench, "mutable.ArrayDequeBench", args.toArray)
