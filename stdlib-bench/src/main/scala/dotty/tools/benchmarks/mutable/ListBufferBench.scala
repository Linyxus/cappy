package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

@State(Scope.Thread)
class ListBufferBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var buf: ListBuffer[Int] = uninitialized

  @Setup
  def setup(): Unit =
    buf = new ListBuffer[Int]
    var i = 0
    while i < size do
      buf += i
      i += 1

  @Benchmark
  def build(): ListBuffer[Int] =
    val b = new ListBuffer[Int]
    var i = 0
    while i < size do
      b += i
      i += 1
    b

  // ListBuffer has no O(1) random access; `toList` is its idiomatic read path.
  @Benchmark
  def access(): List[Int] =
    buf.toList

  @Benchmark
  def transform(): ListBuffer[Int] =
    buf.map(_ + 1)

  // Prepend + remove restores state. Measures prepend + head-remove cost.
  @Benchmark
  def mutate(): Unit =
    0 +=: buf
    buf.remove(0)
