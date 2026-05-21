package dotty.tools.benchmarks.py.interp

/** An elementary 1D cellular-automaton stepper (Rule 90 / Rule 110).
 *  Pure nested `while` loops over `Array[Int]` with bit-twiddling
 *  neighborhood lookups — no ADT, no allocation in the hot path. */
class CellularAutomatonBench:
  var size: Int = 0
  var steps: Int = 0
  var tape: Array[Int] = Array.empty
  var next: Array[Int] = Array.empty
  val rule90: Array[Int]  = Array(0, 1, 1, 0, 1, 0, 0, 1)
  val rule110: Array[Int] = Array(0, 1, 1, 1, 0, 1, 1, 0)

  def setup(size: Int): Unit =
    this.size = size
    steps = math.max(1, size / 8)
    val t = new Array[Int](size)
    var i = 0
    while i < size do { t(i) = (if i % 3 == 0 then 1 else 0); i += 1 }
    tape = t
    next = new Array[Int](size)

  private def step(rule: Array[Int]): Int =
    // Reset tape to initial pattern so repeated op invocations are deterministic.
    var i = 0
    while i < size do { tape(i) = (if i % 3 == 0 then 1 else 0); i += 1 }
    var gen = 0
    while gen < steps do
      var k = 0
      while k < size do
        val l = if k == 0 then 0 else tape(k - 1)
        val c = tape(k)
        val r = if k == size - 1 then 0 else tape(k + 1)
        next(k) = rule((l << 2) | (c << 1) | r)
        k += 1
      var m = 0
      while m < size do { tape(m) = next(m); m += 1 }
      gen += 1
    tape(size / 2)

  val operations: Map[String, () => Any] = Map(
    "rule90"  -> (() => step(rule90)),
    "rule110" -> (() => step(rule110)),
  )

@main def main(): Unit = ()
