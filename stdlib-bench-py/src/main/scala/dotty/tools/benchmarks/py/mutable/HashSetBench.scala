package dotty.tools.benchmarks.py.mutable

import scala.collection.mutable.HashSet
import scala.compiletime.uninitialized

class HashSetBench:
  var size: Int = 0
  var set:  HashSet[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    set = new HashSet[Int]
    var i = 0
    while i < size do
      set += i
      i += 1
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val s = new HashSet[Int]
      var i = 0
      while i < size do
        s += i
        i += 1
      s
    },
    "access"    -> (() => set.contains(half)),
    "transform" -> (() => set.map(_ + 1)),
    "mutate"    -> (() => set += half),
  )

@main def main(): Unit = ()
