package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.collection.immutable.TreeMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
class TreeMapBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var tm: TreeMap[Int, Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    tm = TreeMap.from((0 until size).map(i => i -> i))
    half = size / 2

  @Benchmark
  def build(): TreeMap[Int, Int] =
    TreeMap.from((0 until size).map(i => i -> i))

  @Benchmark
  def access(): Option[Int] =
    tm.get(half)

  @Benchmark
  def transform(): Map[Int, Int] =
    tm.map((k, v) => (k, v + 1))

  @Benchmark
  def mutate(): TreeMap[Int, Int] =
    tm.updated(-1, -1)
