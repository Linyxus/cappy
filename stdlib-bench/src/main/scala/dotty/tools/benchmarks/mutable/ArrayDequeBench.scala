package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.ArrayDeque
import scala.compiletime.uninitialized

@State(Scope.Thread)
class ArrayDequeBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var deque: ArrayDeque[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    deque = new ArrayDeque[Int](size)
    var i = 0
    while i < size do
      deque += i
      i += 1
    half = size / 2

  @Benchmark
  def build(): ArrayDeque[Int] =
    val d = new ArrayDeque[Int](size)
    var i = 0
    while i < size do
      d += i
      i += 1
    d

  @Benchmark
  def access(): Int =
    deque(half)

  @Benchmark
  def transform(): ArrayDeque[Int] =
    deque.map(_ + 1)

  // Idempotent: overwrite slot with its current value.
  @Benchmark
  def mutate(): Unit =
    deque(half) = deque(half)
