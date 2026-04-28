package dotty.tools.benchmarks.py.immutable

import scala.collection.immutable.HashMap
import scala.compiletime.uninitialized

class HashMapBench:
  var size: Int = 0
  var map:  HashMap[Int, Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    map = HashMap.from((0 until size).map(i => i -> i))
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => HashMap.from((0 until size).map(i => i -> i))),
    "access"    -> (() => map.get(half)),
    "transform" -> (() => map.map((k, v) => (k, v + 1))),
    "mutate"    -> (() => map.updated(-1, -1)),
  )

@main def main(): Unit = ()
