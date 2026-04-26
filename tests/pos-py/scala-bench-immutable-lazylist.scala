// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/LazyListBench.scala
// at fixed size = 16.

@main def scalaBenchImmutableLazyList(): Unit =
  val size = 16
  val half = size / 2

  val ll = LazyList.range(0, size)
  ll.length // force, mirroring bench's @Setup

  // build
  val built = LazyList.range(0, size)
  built.length
  println("build:" + built.length + ":" + built.head + ":" + built.last)

  // access
  println("access:" + ll(half))

  // transform: ll.map(_+1).sum returns Int
  println("transform:" + ll.map(_ + 1).sum)

  // mutate (#:: prepend)
  val prepended = 0 #:: ll
  println("mutate:" + prepended.head + ":" + prepended.length)
