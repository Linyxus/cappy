package dotty.tools.benchmarks.mutable

import org.openjdk.jmh.annotations.*
import scala.collection.mutable.StringBuilder
import scala.compiletime.uninitialized

@State(Scope.Thread)
class StringBuilderBench:
  @Param(Array("16", "256", "4096"))
  var size: Int = 0

  var sb: StringBuilder = uninitialized
  var half: Int = 0

  @Setup
  def setup(): Unit =
    sb = new StringBuilder(size)
    var i = 0
    while i < size do
      sb.append('a')
      i += 1
    half = size / 2

  @Benchmark
  def build(): StringBuilder =
    val b = new StringBuilder(size)
    var i = 0
    while i < size do
      b.append('a')
      i += 1
    b

  @Benchmark
  def access(): Char =
    sb.charAt(half)

  @Benchmark
  def transform(): String =
    sb.toString

  // Idempotent: overwrite char with its current value. Returns the read
  // value so JMH consumes it via the auto-Blackhole on @Benchmark return,
  // preventing redundant-store elimination from deleting the body.
  @Benchmark
  def mutate(): Char =
    val v = sb.charAt(half)
    sb.setCharAt(half, v)
    v
