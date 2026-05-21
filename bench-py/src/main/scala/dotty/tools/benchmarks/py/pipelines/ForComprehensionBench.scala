package dotty.tools.benchmarks.py.pipelines

/** For-comprehension desugaring. Two generators desugar to `flatMap` of a
 *  `map`; a guarded single generator desugars via `withFilter`. */
class ForComprehensionBench:
  var size: Int = 0
  var outer: Vector[Int] = Vector.empty
  var inner: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    outer = (0 until 32).toVector
    inner = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "forYieldTwo" -> (() => (for x <- outer; y <- inner yield x + y).sum),
    "forYieldIf"  -> (() => (for x <- inner if x % 3 == 0 yield x * 2).sum),
    "withFilter"  -> (() => inner.withFilter(_ % 3 == 0).map(_ * 2).foldLeft(0L)(_ + _)),
  )

@main def main(): Unit = ()
