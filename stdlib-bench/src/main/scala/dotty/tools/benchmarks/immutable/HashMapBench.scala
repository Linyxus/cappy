package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.collection.immutable.HashMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
class HashMapBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var map: HashMap[Int, Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    map = HashMap.from((0 until size).map(i => i -> i))
    half = size / 2

  @Benchmark
  def build(): HashMap[Int, Int] =
    HashMap.from((0 until size).map(i => i -> i))

  @Benchmark
  def access(): Option[Int] =
    map.get(half)

  @Benchmark
  def transform(): Map[Int, Int] =
    map.map((k, v) => (k, v + 1))

  @Benchmark
  def mutate(): HashMap[Int, Int] =
    map.updated(-1, -1)
