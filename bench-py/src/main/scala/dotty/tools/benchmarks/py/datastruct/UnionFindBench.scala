package dotty.tools.benchmarks.py.datastruct

/** Path-compressed union-find over two Array[Int]. Tight array read/write
 *  in path rewriting, recursive find, no allocation after setup. */
class UnionFindBench:
  var size: Int = 0
  var parent: Array[Int] = new Array[Int](1)
  var rank: Array[Int] = new Array[Int](1)

  def find(par: Array[Int], i: Int): Int =
    if par(i) != i then
      par(i) = find(par, par(i))
      par(i)
    else i

  def union(par: Array[Int], rk: Array[Int], a: Int, b: Int): Unit =
    val ra = find(par, a)
    val rb = find(par, b)
    if ra != rb then
      if rk(ra) < rk(rb) then par(ra) = rb
      else if rk(ra) > rk(rb) then par(rb) = ra
      else { par(rb) = ra; rk(ra) += 1 }

  def freshParent(n: Int): Array[Int] =
    val par = new Array[Int](n)
    var i = 0
    while i < n do { par(i) = i; i += 1 }
    par

  def setup(size: Int): Unit =
    this.size = size
    parent = freshParent(size)
    rank = new Array[Int](size)
    var i = 0
    while i < size - 1 do
      if i % 3 != 0 then union(parent, rank, i, i + 1)
      i += 1

  val operations: Map[String, () => Any] = Map(
    "findAll" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += find(parent, i).toLong
        i += 1
      s
    },
    "unionBatch" -> { () =>
      val par = freshParent(size)
      val rk = new Array[Int](size)
      var x = 99
      var i = 0
      while i < size do
        x = x * 1664525 + 1013904223
        val a = (x & 0x7fffffff) % size
        x = x * 1664525 + 1013904223
        val b = (x & 0x7fffffff) % size
        union(par, rk, a, b)
        i += 1
      find(par, 0)
    },
    "connected" -> { () =>
      var count = 0
      var i = 0
      while i < size do
        if find(parent, i) == find(parent, size - 1 - i) then count += 1
        i += 1
      count
    },
  )

@main def main(): Unit = ()
