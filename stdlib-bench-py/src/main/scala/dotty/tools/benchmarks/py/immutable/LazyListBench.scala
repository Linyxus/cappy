package dotty.tools.benchmarks.py.immutable

import scala.compiletime.uninitialized

class LazyListBench:
  var size: Int = 0
  var ll:   LazyList[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    ll = LazyList.range(0, size)
    ll.length
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => LazyList.range(0, size)),
    "access"    -> (() => ll(half)),
    "transform" -> (() => ll.map(_ + 1).sum),
    "mutate"    -> (() => 0 #:: ll),
  )

@main def main(): Unit = ()
