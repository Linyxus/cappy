// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/HashSetBench.scala
// at fixed size = 16.

import scala.collection.mutable.HashSet

@main def scalaBenchMutableHashSet(): Unit =
  val size = 16
  val half = size / 2

  val set = new HashSet[Int]
  var i = 0
  while i < size do
    set += i
    i += 1

  // build
  val built = new HashSet[Int]
  i = 0
  while i < size do
    built += i
    i += 1
  println("build:" + built.size)

  // access
  println("access:" + set.contains(half))

  // transform
  val mapped = set.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped.contains(half + 1))

  // mutate (idempotent: adding an element that's already present is a no-op)
  set += half
  println("mutate:" + set.size + ":" + set.contains(half))
