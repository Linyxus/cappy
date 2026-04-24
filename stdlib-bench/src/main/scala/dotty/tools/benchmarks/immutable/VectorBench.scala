package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

@State(Scope.Thread)
class VectorBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var vec: Vector[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    vec = (0 until size).toVector
    half = size / 2

  @Benchmark
  def build(): Vector[Int] =
    (0 until size).toVector

  @Benchmark
  def access(): Int =
    vec(half)

  @Benchmark
  def transform(): Vector[Int] =
    vec.map(_ + 1)

  @Benchmark
  def mutate(): Vector[Int] =
    vec :+ 0
