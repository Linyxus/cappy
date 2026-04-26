// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/HashMapBench.scala
// at fixed size = 16. Same Range.map sidestep as the TreeMap test
// (literal List of 16 pairs instead of `(0 until size).map(i => i -> i)`).

import scala.collection.immutable.HashMap

@main def scalaBenchImmutableHashMap(): Unit =
  val size = 16
  val half = size / 2

  val pairs = List(
    (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7),
    (8, 8), (9, 9), (10, 10), (11, 11), (12, 12), (13, 13), (14, 14), (15, 15)
  )
  val m = HashMap.from(pairs)

  // build
  val built = HashMap.from(pairs)
  println("build:" + built.size)

  // access
  println("access:" + m.get(half))

  // transform
  val mapped = m.map((k, v) => (k, v + 1))
  println("transform:" + mapped.size + ":" + mapped.get(half))

  // mutate
  val updated = m.updated(-1, -1)
  println("mutate:" + updated.size + ":" + updated.get(-1))
