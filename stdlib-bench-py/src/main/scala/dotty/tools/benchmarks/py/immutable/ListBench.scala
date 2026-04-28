package dotty.tools.benchmarks.py.immutable

import scala.compiletime.uninitialized

class ListBench:
  var size: Int = 0
  var list: List[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    list = List.range(0, size)
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => List.range(0, size)),
    "access"    -> (() => list(half)),
    "transform" -> (() => list.map(_ + 1)),
    "mutate"    -> (() => 0 :: list),
  )

@main def main(): Unit = ()
