// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/HashMapBench.scala
// at fixed size = 16.

import scala.collection.mutable.HashMap

@main def scalaBenchMutableHashMap(): Unit =
  val size = 16
  val half = size / 2

  val m = new HashMap[Int, Int]
  var i = 0
  while i < size do
    m(i) = i
    i += 1

  // build
  val built = new HashMap[Int, Int]
  i = 0
  while i < size do
    built(i) = i
    i += 1
  println("build:" + built.size)

  // access
  println("access:" + m.get(half))

  // transform
  val mapped = m.map((k, v) => (k, v + 1))
  println("transform:" + mapped.size + ":" + mapped.get(half))

  // mutate (idempotent: overwrite existing key with the same value).
  // The RHS `m(half)` exercises the specialized `apply_mcII_sp__I__I`
  // path that goes through the runtime `Function1` apply bridge.
  m(half) = m(half)
  println("mutate:" + m.size + ":" + m.get(half))
