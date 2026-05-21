package dotty.tools.benchmarks.py.mathalg

/** Iterative Euclid GCD/LCM over pairs. Deeply nested `while`; every `%`
 *  and `/` routes through the truncating runtime helpers, every mutation
 *  re-wraps through `_scpy_i32`. */
class GcdBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "gcdLoop" -> { () =>
      var last = 0
      var k = 1
      while k <= size do
        var a = size
        var b = k
        while b != 0 do
          val t = b
          b = a % b
          a = t
        last = a
        k += 1
      last
    },
    "lcmLoop" -> { () =>
      var acc = 0
      var k = 1
      while k < size do
        val a = k
        val b = k + 1
        var aa = a
        var bb = b
        while bb != 0 do
          val t = bb
          bb = aa % bb
          aa = t
        val g = aa
        acc += (a / g) * b % size
        k += 1
      acc
    },
  )

@main def main(): Unit = ()
