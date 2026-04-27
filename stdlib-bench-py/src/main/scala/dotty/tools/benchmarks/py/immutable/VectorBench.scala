package dotty.tools.benchmarks.py.immutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.compiletime.uninitialized

class VectorBench extends BenchmarkSuite:
  var size: Int = 0
  var vec:  Vector[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    vec = (0 until size).toVector
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build"     -> (() => (0 until size).toVector),
    "access"    -> (() => vec(half)),
    "transform" -> (() => vec.map(_ + 1)),
    "mutate"    -> (() => vec :+ 0),
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new VectorBench, "immutable.VectorBench", args.toArray)
