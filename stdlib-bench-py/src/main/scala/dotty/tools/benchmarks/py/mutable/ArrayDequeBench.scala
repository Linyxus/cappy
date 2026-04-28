package dotty.tools.benchmarks.py.mutable

import scala.collection.mutable.ArrayDeque
import scala.compiletime.uninitialized

class ArrayDequeBench:
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
    "mutate"    -> { () =>
      val v = deque(half)
      deque(half) = v
      v
    },
  )

@main def main(): Unit = ()
