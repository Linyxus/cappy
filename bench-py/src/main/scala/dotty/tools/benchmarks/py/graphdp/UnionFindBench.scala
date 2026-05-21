package dotty.tools.benchmarks.py.graphdp

/** Connected components via union-find with path-halving and union by rank.
 *  Uses a nested local `def find` inside the op to stress the closure /
 *  local-method call path. Edges are generated deterministically. */
class UnionFindBench:
  var size: Int = 0
  var edgesU: Array[Int] = Array.empty
  var edgesV: Array[Int] = Array.empty
  var numEdges: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    val m = size * 2
    this.numEdges = m
    val u = new Array[Int](m)
    val v = new Array[Int](m)
    var k = 0
    while k < m do
      u(k) = k % size
      v(k) = (k * 3 + 7) % size
      k += 1
    this.edgesU = u
    this.edgesV = v

  val operations: Map[String, () => Any] = Map(
    "unionFind" -> { () =>
      val parent = new Array[Int](size)
      val rank = new Array[Int](size)
      var i = 0
      while i < size do
        parent(i) = i
        i += 1

      def find(x: Int): Int =
        var node = x
        while parent(node) != node do
          parent(node) = parent(parent(node))
          node = parent(node)
        node

      var k = 0
      while k < numEdges do
        val ru = find(edgesU(k))
        val rv = find(edgesV(k))
        if ru != rv then
          if rank(ru) < rank(rv) then parent(ru) = rv
          else if rank(ru) > rank(rv) then parent(rv) = ru
          else
            parent(rv) = ru
            rank(ru) += 1
        k += 1

      var components = 0
      i = 0
      while i < size do
        if find(i) == i then components += 1
        i += 1
      components
    },
  )

@main def main(): Unit = ()
