// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/HashSetBench.scala
// at fixed size = 16.

import scala.collection.immutable.HashSet

@main def scalaBenchImmutableHashSet(): Unit =
  val size = 16
  val half = size / 2

  val set = HashSet.from(0 until size)

  // build
  val built = HashSet.from(0 until size)
  println("build:" + built.size)

  // access
  println("access:" + set.contains(half))

  // transform
  val mapped = set.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped.contains(half + 1))

  // mutate (insert -1)
  val withNeg = set + (-1)
  println("mutate:" + withNeg.size + ":" + withNeg.contains(-1))
