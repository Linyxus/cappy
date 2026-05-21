package dotty.tools.benchmarks.py.sorting

/** Hand-rolled binary search vs linear search over a sorted `Array[Int]`. The
 *  array is read-only across ops, so no per-call re-init is needed. */
class BinarySearchBench:
  var size: Int = 0
  var sorted: Array[Int] = new Array[Int](0)
  var target: Int = 0
  var missing: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    sorted = new Array[Int](size)
    var i = 0
    while i < size do
      sorted(i) = i * 2          // even values 0,2,4,...
      i += 1
    target = (size / 2) * 2      // an even value that is present
    missing = size * 2 + 1       // odd value, never present

  val operations: Map[String, () => Any] = Map(
    "binarySearchHit" -> { () =>
      var lo = 0
      var hi = size - 1
      var result = -1
      while lo <= hi do
        val mid = lo + (hi - lo) / 2
        val v = sorted(mid)
        if v == target then { result = mid; lo = hi + 1 }
        else if v < target then lo = mid + 1
        else hi = mid - 1
      result
    },
    "binarySearchMiss" -> { () =>
      var lo = 0
      var hi = size - 1
      var result = -1
      while lo <= hi do
        val mid = lo + (hi - lo) / 2
        val v = sorted(mid)
        if v == missing then { result = mid; lo = hi + 1 }
        else if v < missing then lo = mid + 1
        else hi = mid - 1
      result
    },
    "linearSearch" -> { () =>
      var i = 0
      var result = -1
      while i < size && result < 0 do
        if sorted(i) == target then result = i
        i += 1
      result
    },
  )

@main def main(): Unit = ()
