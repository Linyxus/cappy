// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/ListBufferBench.scala
// at fixed size = 16.

import scala.collection.mutable.ListBuffer

@main def scalaBenchMutableListBuffer(): Unit =
  val size = 16

  val buf = new ListBuffer[Int]
  var i = 0
  while i < size do
    buf += i
    i += 1

  // build
  val built = new ListBuffer[Int]
  i = 0
  while i < size do
    built += i
    i += 1
  println("build:" + built.size + ":" + built.head + ":" + built.last)

  // access (toList — bench's idiomatic O(1) read path)
  val accessed = buf.toList
  println("access:" + accessed.size + ":" + accessed.head + ":" + accessed.last)

  // transform
  val mapped = buf.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped.head + ":" + mapped.last)

  // mutate (prepend then drop the prepended element — bench restores state)
  0 +=: buf
  buf.remove(0)
  println("mutate:" + buf.size + ":" + buf.head + ":" + buf.last)
