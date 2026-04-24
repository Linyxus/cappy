package dotty.tools.benchmarks.immutable

import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

@State(Scope.Thread)
class LazyListBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  // Pre-forced: `@Setup` iterates the lazy list so access/transform measure
  // steady-state cost rather than first-force cost.
  var ll: LazyList[Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    ll = LazyList.range(0, size)
    ll.length
    half = size / 2

  @Benchmark
  def build(): LazyList[Int] =
    LazyList.range(0, size)

  @Benchmark
  def access(): Int =
    ll(half)

  @Benchmark
  def transform(): Int =
    ll.map(_ + 1).sum

  @Benchmark
  def mutate(): LazyList[Int] =
    0 #:: ll
