package dotty.tools.benchmarks.py.immutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.immutable.TreeMap
import scala.compiletime.uninitialized

class TreeMapBench extends BenchmarkSuite:
  var size: Int = 0
  var tm:   TreeMap[Int, Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    tm = TreeMap.from((0 until size).map(i => i -> i))
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => TreeMap.from((0 until size).map(i => i -> i))),
    "access"    -> (() => tm.get(half)),
    "transform" -> (() => tm.map((k, v) => (k, v + 1))),
    "mutate"    -> (() => tm.updated(-1, -1)),
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new TreeMapBench, "immutable.TreeMapBench", args.toArray)
