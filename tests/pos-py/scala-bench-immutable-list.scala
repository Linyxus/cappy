// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/immutable/ListBench.scala
// at fixed size = 16.

@main def scalaBenchImmutableList(): Unit =
  val size = 16
  val half = size / 2

  val list = List.range(0, size)

  // build
  val built = List.range(0, size)
  println("build:" + built.length + ":" + built.head + ":" + built.last)

  // access (List.apply is O(n))
  println("access:" + list(half))

  // transform
  val mapped = list.map(_ + 1)
  println("transform:" + mapped.length + ":" + mapped.head + ":" + mapped.last)

  // mutate (prepend)
  val prepended = 0 :: list
  println("mutate:" + prepended.length + ":" + prepended.head)
