package dotty.tools.benchmarks.py.mutable

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class ArrayBufferBench:
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
    "mutate"    -> { () =>
      val v = buf(half)
      buf(half) = v
      v
    },
  )

@main def main(): Unit = ()
