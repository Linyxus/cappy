package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.collection.immutable.HashSet
import scala.compiletime.uninitialized

@State(Scope.Thread)
class HashSetBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var set: HashSet[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    set = HashSet.from(0 until size)
    half = size / 2

  @Benchmark
  def build(): HashSet[Int] =
    HashSet.from(0 until size)

  @Benchmark
  def access(): Boolean =
    set.contains(half)

  @Benchmark
  def transform(): Set[Int] =
    set.map(_ + 1)

  @Benchmark
  def mutate(): HashSet[Int] =
    set + (-1)
