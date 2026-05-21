package dotty.tools.benchmarks.py.graphdp

/** Floyd-Warshall all-pairs shortest paths on a capped NxN matrix stored
 *  as a flattened `Array[Int]`. Dimension `n` is capped so `n^3` stays
 *  bounded even at large `size`. */
class FloydWarshallBench:
  var n: Int = 0
  var dist: Array[Int] = Array.empty

  private final val Inf = Int.MaxValue / 2

  def setup(size: Int): Unit =
    val dim = math.min(size / 8, 48)
    this.n = dim
    val d = new Array[Int](dim * dim)
    var i = 0
    while i < dim do
      var j = 0
      while j < dim do
        d(i * dim + j) = if i == j then 0 else Inf
        j += 1
      i += 1
    // sparse ring edges
    i = 0
    while i < dim do
      d(i * dim + ((i + 1) % dim)) = (i % 7) + 1
      i += 1
    this.dist = d

  val operations: Map[String, () => Any] = Map(
    "floydWarshall" -> { () =>
      val d = dist.clone()
      var k = 0
      while k < n do
        var i = 0
        while i < n do
          var j = 0
          while j < n do
            val via = d(i * n + k) + d(k * n + j)
            if via < d(i * n + j) then d(i * n + j) = via
            j += 1
          i += 1
        k += 1
      d(0 * n + (n - 1))
    },
  )

@main def main(): Unit = ()
