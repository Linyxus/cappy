package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

@State(Scope.Thread)
class ListBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var list: List[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    list = List.range(0, size)
    half = size / 2

  @Benchmark
  def build(): List[Int] =
    List.range(0, size)

  @Benchmark
  def access(): Int =
    list(half)

  @Benchmark
  def transform(): List[Int] =
    list.map(_ + 1)

  @Benchmark
  def mutate(): List[Int] =
    0 :: list
