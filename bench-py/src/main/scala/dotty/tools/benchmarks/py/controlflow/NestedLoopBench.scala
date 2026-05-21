package dotty.tools.benchmarks.py.controlflow

/** Nested-loop matrix scan vs. a flattened single loop, doing the same total
 *  work. `side` is an integer formula (~sqrt(size)) so total iterations stay
 *  ~size. Stresses nested `foreach`/`while` depth in the emitted Python. */
class NestedLoopBench:
  var size: Int = 0
  var side: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    // Integer approximation of sqrt(size) without scala.math.
    var s = 0
    while (s + 1) * (s + 1) <= size do s += 1
    side = if s < 1 then 1 else s

  val operations: Map[String, () => Any] = Map(
    "nestedWhile" -> { () =>
      var sum = 0
      var i = 0
      while i < side do
        var j = 0
        while j < side do
          sum += i * side + j
          j += 1
        i += 1
      sum
    },
    "nestedForEach" -> { () =>
      var sum = 0
      (0 until side).foreach(i => (0 until side).foreach(j => sum += i * side + j))
      sum
    },
    "flatWhile" -> { () =>
      val total = side * side
      var sum = 0
      var k = 0
      while k < total do
        sum += k
        k += 1
      sum
    },
    "nestedFoldLeft" -> { () =>
      (0 until side).foldLeft(0)((acc, i) =>
        acc + (0 until side).foldLeft(0)((a, j) => a + i * side + j))
    },
  )

@main def main(): Unit = ()
