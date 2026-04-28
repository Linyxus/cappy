package dotty.tools.benchmarks.py.immutable

import scala.collection.immutable.HashSet
import scala.compiletime.uninitialized

class HashSetBench:
  var size: Int = 0
  var set:  HashSet[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    set = HashSet.from(0 until size)
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => HashSet.from(0 until size)),
    "access"    -> (() => set.contains(half)),
    "transform" -> (() => set.map(_ + 1)),
    "mutate"    -> (() => set + (-1)),
  )

@main def main(): Unit = ()
