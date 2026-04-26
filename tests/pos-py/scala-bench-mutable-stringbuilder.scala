// Mirrors stdlib-bench/src/main/scala/dotty/tools/benchmarks/mutable/StringBuilderBench.scala
// at fixed size = 16. Uses scala.collection.mutable.StringBuilder (not java.lang).

import scala.collection.mutable.StringBuilder

@main def scalaBenchMutableStringBuilder(): Unit =
  val size = 16
  val half = size / 2

  val sb = new StringBuilder(size)
  var i = 0
  while i < size do
    sb.append('a')
    i += 1

  // build
  val built = new StringBuilder(size)
  i = 0
  while i < size do
    built.append('a')
    i += 1
  println("build:" + built.length + ":" + built.charAt(0) + ":" + built.charAt(size - 1))

  // access
  println("access:" + sb.charAt(half))

  // transform (toString)
  val s = sb.toString
  println("transform:" + s.length + ":" + s.charAt(0) + ":" + s.charAt(size - 1))

  // mutate
  val mutated = new StringBuilder(size)
  i = 0
  while i < size do
    mutated.append('a')
    i += 1
  mutated.setCharAt(0, 'Z')
  mutated.setCharAt(half, 'Y')
  println("mutate:" + mutated.charAt(0) + ":" + mutated.charAt(half) + ":" + mutated.charAt(size - 1))
