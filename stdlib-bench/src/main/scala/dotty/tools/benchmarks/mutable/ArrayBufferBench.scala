package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

@State(Scope.Thread)
class ArrayBufferBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var buf: ArrayBuffer[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    buf = new ArrayBuffer[Int](size)
    var i = 0
    while i < size do
      buf += i
      i += 1
    half = size / 2

  @Benchmark
  def build(): ArrayBuffer[Int] =
    val b = new ArrayBuffer[Int](size)
    var i = 0
    while i < size do
      b += i
      i += 1
    b

  @Benchmark
  def access(): Int =
    buf(half)

  @Benchmark
  def transform(): ArrayBuffer[Int] =
    buf.map(_ + 1)

  // Idempotent: overwrite slot with its current value.
  @Benchmark
  def mutate(): Unit =
    buf(half) = buf(half)
