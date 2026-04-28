package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.HashMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
class HashMapBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var map: HashMap[Int, Int] = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    map = new HashMap[Int, Int]
    var i = 0
    while i < size do
      map(i) = i
      i += 1
    half = size / 2

  @Benchmark
  def build(): HashMap[Int, Int] =
    val m = new HashMap[Int, Int]
    var i = 0
    while i < size do
      m(i) = i
      i += 1
    m

  @Benchmark
  def access(): Option[Int] =
    map.get(half)

  @Benchmark
  def transform(): HashMap[Int, Int] =
    map.map((k, v) => (k, v + 1))

  // Idempotent: overwrite existing key with the same value. Returns the
  // read value so JMH consumes it via the auto-Blackhole on @Benchmark
  // return, preventing redundant-store elimination from deleting the body.
  @Benchmark
  def mutate(): Int =
    val v = map(half)
    map(half) = v
    v
