package dotty.tools.benchmarks.py.graphdp

/** Longest increasing subsequence via the O(n log n) patience-sort method:
 *  a `tails` array maintained by binary search. Input is a deterministic
 *  pseudo-random `Int` sequence built with a linear congruential formula. */
class LisBench:
  var size: Int = 0
  var seq: Array[Int] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val a = new Array[Int](size)
    var i = 0
    while i < size do
      a(i) = ((i * 1664525 + 1013904223) >>> 1) % (size * 2)
      i += 1
    this.seq = a

  val operations: Map[String, () => Any] = Map(
    "lisLength" -> { () =>
      val tails = new Array[Int](size)
      var len = 0
      var i = 0
      while i < size do
        val x = seq(i)
        var lo = 0
        var hi = len
        while lo < hi do
          val mid = (lo + hi) >>> 1
          if tails(mid) < x then lo = mid + 1 else hi = mid
        tails(lo) = x
        if lo == len then len += 1
        i += 1
      len
    },
  )

@main def main(): Unit = ()
