// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/ArrayBufferBench.scala
// at fixed size = 16. One println per benchmark op (build/access/transform/mutate).

import scala.collection.mutable.ArrayBuffer

@main def scalaBenchMutableArrayBuffer(): Unit =
  val size = 16
  val half = size / 2

  val buf = new ArrayBuffer[Int](size)
  var i = 0
  while i < size do
    buf += i
    i += 1

  // build
  val built = new ArrayBuffer[Int](size)
  i = 0
  while i < size do
    built += i
    i += 1
  println("build:" + built.size + ":" + built(0) + ":" + built(size - 1))

  // access
  println("access:" + buf(half))

  // transform
  val mapped = buf.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped(0) + ":" + mapped(half) + ":" + mapped(size - 1))

  // mutate (idempotent)
  buf(half) = buf(half)
  println("mutate:" + buf(half) + ":" + buf.size)
