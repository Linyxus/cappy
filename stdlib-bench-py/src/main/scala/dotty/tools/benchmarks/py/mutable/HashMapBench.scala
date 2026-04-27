package dotty.tools.benchmarks.py.mutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.mutable.HashMap
import scala.compiletime.uninitialized

class HashMapBench extends BenchmarkSuite:
  var size: Int = 0
  var map:  HashMap[Int, Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    map = new HashMap[Int, Int]
    var i = 0
    while i < size do
      map(i) = i
      i += 1
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val m = new HashMap[Int, Int]
      var i = 0
      while i < size do
        m(i) = i
        i += 1
      m
    },
    "access"    -> (() => map.get(half)),
    "transform" -> (() => map.map((k, v) => (k, v + 1))),
    "mutate"    -> { () => map(half) = map(half) },
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new HashMapBench, "mutable.HashMapBench", args.toArray)
