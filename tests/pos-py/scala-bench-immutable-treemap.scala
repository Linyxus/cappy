// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/TreeMapBench.scala
// at fixed size = 16. Bench builds via `(0 until size).map(i => i -> i)`; the
// test substitutes a literal `List[(Int, Int)]` of 16 pairs to keep the input
// independent of Range/closure machinery.

import scala.collection.immutable.TreeMap

@main def scalaBenchImmutableTreeMap(): Unit =
  val size = 16
  val half = size / 2

  val pairs = List(
    (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7),
    (8, 8), (9, 9), (10, 10), (11, 11), (12, 12), (13, 13), (14, 14), (15, 15)
  )
  val tm = TreeMap.from(pairs)

  // build
  val built = TreeMap.from(pairs)
  println("build:" + built.size)

  // access
  println("access:" + tm.get(half))

  // transform
  val mapped = tm.map((k, v) => (k, v + 1))
  println("transform:" + mapped.size + ":" + mapped.get(half))

  // mutate (insert key -1, value -1)
  val updated = tm.updated(-1, -1)
  println("mutate:" + updated.size + ":" + updated.get(-1))
