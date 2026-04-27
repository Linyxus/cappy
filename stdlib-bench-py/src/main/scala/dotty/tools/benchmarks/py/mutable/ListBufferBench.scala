package dotty.tools.benchmarks.py.mutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

class ListBufferBench extends BenchmarkSuite:
  var size: Int = 0
  var buf:  ListBuffer[Int] = uninitialized

  def setup(size: Int): Unit =
    this.size = size
    buf = new ListBuffer[Int]
    var i = 0
    while i < size do
      buf += i
      i += 1

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val b = new ListBuffer[Int]
      var i = 0
      while i < size do
        b += i
        i += 1
      b
    },
    "access"    -> (() => buf.toList),
    "transform" -> (() => buf.map(_ + 1)),
    "mutate"    -> { () =>
      0 +=: buf
      buf.remove(0)
    },
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new ListBufferBench, "mutable.ListBufferBench", args.toArray)
