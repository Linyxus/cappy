package dotty.tools.benchmarks.py.mutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.mutable.StringBuilder
import scala.compiletime.uninitialized

class StringBuilderBench extends BenchmarkSuite:
  var size: Int = 0
  var sb:   StringBuilder = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    sb = new StringBuilder(size)
    var i = 0
    while i < size do
      sb.append('a')
      i += 1
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val b = new StringBuilder(size)
      var i = 0
      while i < size do
        b.append('a')
        i += 1
      b
    },
    "access"    -> (() => sb.charAt(half)),
    "transform" -> (() => sb.toString),
    "mutate"    -> { () => sb.setCharAt(half, sb.charAt(half)) },
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new StringBuilderBench, "mutable.StringBuilderBench", args.toArray)
