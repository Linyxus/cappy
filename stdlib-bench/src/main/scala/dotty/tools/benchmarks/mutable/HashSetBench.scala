package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.HashSet
import scala.compiletime.uninitialized

@State(Scope.Thread)
class HashSetBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var set: HashSet[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    set = new HashSet[Int]
    var i = 0
    while i < size do
      set += i
      i += 1
    half = size / 2

  @Benchmark
  def build(): HashSet[Int] =
    val s = new HashSet[Int]
    var i = 0
    while i < size do
      s += i
      i += 1
    s

  @Benchmark
  def access(): Boolean =
    set.contains(half)

  @Benchmark
  def transform(): HashSet[Int] =
    set.map(_ + 1)

  // Idempotent: adding an element that's already present is a no-op.
  @Benchmark
  def mutate(): HashSet[Int] =
    set += half
