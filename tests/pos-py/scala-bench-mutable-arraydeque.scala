// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/ArrayDequeBench.scala
// at fixed size = 16.

import scala.collection.mutable.ArrayDeque

@main def scalaBenchMutableArrayDeque(): Unit =
  val size = 16
  val half = size / 2

  val deque = new ArrayDeque[Int](size)
  var i = 0
  while i < size do
    deque += i
    i += 1

  // build
  val built = new ArrayDeque[Int](size)
  i = 0
  while i < size do
    built += i
    i += 1
  println("build:" + built.size + ":" + built(0) + ":" + built(size - 1))

  // access
  println("access:" + deque(half))

  // transform
  val mapped = deque.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped(0) + ":" + mapped(size - 1))

  // mutate (idempotent)
  deque(half) = deque(half)
  println("mutate:" + deque(half) + ":" + deque.size)
