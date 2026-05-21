package dotty.tools.benchmarks.py.mathalg

/** Integer square root via Newton's method and bit-by-bit binary search.
 *  Long division in the Newton step and `candidate * candidate` in the
 *  bit method hit the `_scpy_i64` truncation path repeatedly. */
class IntSqrtBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "isqrtNewton" -> { () =>
      var last = 0L
      var n = 1
      while n <= size do
        val target = n.toLong * n * 1000L
        var x = target
        var x1 = (x + 1L) / 2L
        while x1 < x do
          x = x1
          x1 = (x + target / x) / 2L
        last = x
        n += 1
      last
    },
    "isqrtBit" -> { () =>
      var last = 0L
      var n = 1
      while n <= size do
        val s = n.toLong * n * 1000L
        var bit = 1L << 30
        val rem = s
        var root = 0L
        while bit > 0 do
          val candidate = root + bit
          if candidate * candidate <= rem then root = candidate
          bit >>= 1
        last = root
        n += 1
      last
    },
  )

@main def main(): Unit = ()
