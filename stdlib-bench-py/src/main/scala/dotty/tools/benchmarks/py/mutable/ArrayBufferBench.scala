package dotty.tools.benchmarks.py.mutable

import dotty.tools.benchmarks.py.{BenchmarkSuite, Harness}
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class ArrayBufferBench extends BenchmarkSuite:
  var size: Int = 0
  var buf:  ArrayBuffer[Int] = uninitialized
  var half: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    buf = new ArrayBuffer[Int](size)
    var i = 0
    while i < size do
      buf += i
      i += 1
    half = size / 2

  val operations: Map[String, () => Any] = Map(
    "build" -> { () =>
      val b = new ArrayBuffer[Int](size)
      var i = 0
      while i < size do
        b += i
        i += 1
      b
    },
    "access"    -> (() => buf(half)),
    "transform" -> (() => buf.map(_ + 1)),
    "mutate"    -> { () => buf(half) = buf(half) },
  )

@main def main(args: String*): Unit =
  Harness.runFromArgs(new ArrayBufferBench, "mutable.ArrayBufferBench", args.toArray)
