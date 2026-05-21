package dotty.tools.benchmarks.py.graphdp

/** Coin-change DP over a fixed set of denominations. Two related ops:
 *  minimum coins for a target amount, and the count of distinct
 *  combinations. The amount scales with `size`; both DP tables are 1D. */
class CoinChangeBench:
  var size: Int = 0
  var coins: Array[Int] = Array.empty
  var amount: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    this.amount = size
    this.coins = Array(1, 3, 7, 11, 23, 47)

  val operations: Map[String, () => Any] = Map(
    "coinChangeMin" -> { () =>
      val inf = amount + 1
      val dp = new Array[Int](amount + 1)
      var j = 1
      while j <= amount do
        dp(j) = inf
        j += 1
      dp(0) = 0
      var ci = 0
      while ci < coins.length do
        val c = coins(ci)
        j = c
        while j <= amount do
          val prev = dp(j - c) + 1
          if prev < dp(j) then dp(j) = prev
          j += 1
        ci += 1
      dp(amount)
    },
    "coinChangeCount" -> { () =>
      val dp = new Array[Int](amount + 1)
      dp(0) = 1
      var ci = 0
      while ci < coins.length do
        val c = coins(ci)
        var j = c
        while j <= amount do
          dp(j) += dp(j - c)
          j += 1
        ci += 1
      dp(amount)
    },
  )

@main def main(): Unit = ()
