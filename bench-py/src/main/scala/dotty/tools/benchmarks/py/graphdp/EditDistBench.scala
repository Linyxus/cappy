package dotty.tools.benchmarks.py.graphdp

/** Levenshtein edit distance over two integer-alphabet sequences using a
 *  dense 2D DP table flattened into a 1D `Array[Int]`. Sequence length is
 *  `size / 4` so the table stays bounded. */
class EditDistBench:
  var size: Int = 0
  var s: Array[Int] = Array.empty
  var t: Array[Int] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val len = size / 4
    val a = new Array[Int](len)
    val b = new Array[Int](len)
    var i = 0
    while i < len do
      a(i) = i % 26
      b(i) = (i * 3 + 7) % 26
      i += 1
    this.s = a
    this.t = b

  val operations: Map[String, () => Any] = Map(
    "editDist" -> { () =>
      val m = s.length
      val n = t.length
      val dp = new Array[Int]((m + 1) * (n + 1))
      var i = 0
      while i <= m do
        dp(i * (n + 1)) = i
        i += 1
      var j = 0
      while j <= n do
        dp(j) = j
        j += 1
      i = 1
      while i <= m do
        j = 1
        while j <= n do
          val cost = if s(i - 1) == t(j - 1) then 0 else 1
          val a = dp((i - 1) * (n + 1) + j) + 1
          val b = dp(i * (n + 1) + (j - 1)) + 1
          val c = dp((i - 1) * (n + 1) + (j - 1)) + cost
          dp(i * (n + 1) + j) = math.min(a, math.min(b, c))
          j += 1
        i += 1
      dp(m * (n + 1) + n)
    },
  )

@main def main(): Unit = ()
