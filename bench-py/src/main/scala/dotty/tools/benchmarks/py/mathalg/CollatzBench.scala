package dotty.tools.benchmarks.py.mathalg

/** Collatz chain lengths for `[1, size]`. Variable-length inner loop,
 *  alternating `Long` division and multiply in one body, parity test via
 *  `(x & 1L)`. A pure `_scpy_i64` pressure test. */
class CollatzBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "collatzMax" -> { () =>
      var maxLen = 0
      var maxStart = 1
      var n = 1
      while n <= size do
        var x = n.toLong
        var len = 0
        while x != 1L do
          if (x & 1L) == 0L then x /= 2L
          else x = 3L * x + 1L
          len += 1
        if len > maxLen then
          maxLen = len
          maxStart = n
        n += 1
      maxStart
    },
    "collatzSum" -> { () =>
      var total = 0L
      var n = 1
      while n <= size do
        var x = n.toLong
        var len = 0L
        while x != 1L do
          if (x & 1L) == 0L then x /= 2L
          else x = 3L * x + 1L
          len += 1L
        total += len
        n += 1
      total
    },
  )

@main def main(): Unit = ()
