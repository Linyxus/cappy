package dotty.tools.benchmarks.py.graphdp

/** 0/1 knapsack via a rolling 1D DP array. `n` items, capacity scaled by
 *  `size`; the DP buffer is length `capacity + 1`. */
class KnapsackBench:
  var size: Int = 0
  var weights: Array[Int] = Array.empty
  var values: Array[Int] = Array.empty
  var capacity: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    this.capacity = size
    val w = new Array[Int](size)
    val v = new Array[Int](size)
    var i = 0
    while i < size do
      w(i) = (i % 7) + 1
      v(i) = (i % 11) + 1
      i += 1
    this.weights = w
    this.values = v

  val operations: Map[String, () => Any] = Map(
    "knapsack01" -> { () =>
      val dp = new Array[Int](capacity + 1)
      var i = 0
      while i < size do
        val w = weights(i)
        val v = values(i)
        var j = capacity
        while j >= w do
          val prev = dp(j - w) + v
          if prev > dp(j) then dp(j) = prev
          j -= 1
        i += 1
      dp(capacity)
    },
  )

@main def main(): Unit = ()
