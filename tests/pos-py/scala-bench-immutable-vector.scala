// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/VectorBench.scala
// at fixed size = 16.

@main def scalaBenchImmutableVector(): Unit =
  val size = 16
  val half = size / 2

  val vec = (0 until size).toVector

  // build
  val built = (0 until size).toVector
  println("build:" + built.size + ":" + built(0) + ":" + built(size - 1))

  // access
  println("access:" + vec(half))

  // transform
  val mapped = vec.map(_ + 1)
  println("transform:" + mapped.size + ":" + mapped(0) + ":" + mapped(half) + ":" + mapped(size - 1))

  // mutate (append)
  val appended = vec :+ 0
  println("mutate:" + appended.size + ":" + appended(size))
