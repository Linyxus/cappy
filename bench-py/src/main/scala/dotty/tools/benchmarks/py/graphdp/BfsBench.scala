package dotty.tools.benchmarks.py.graphdp

/** Breadth-first traversal over a sparse adjacency list stored as
 *  `Array[List[Int]]`. Each node has exactly two outgoing edges
 *  (ring + skip), so the graph stays linear in `size`. */
class BfsBench:
  var size: Int = 0
  var adj: Array[List[Int]] = Array.empty
  var source: Int = 0

  def setup(size: Int): Unit =
    this.size = size
    this.source = 0
    val a = new Array[List[Int]](size)
    var i = 0
    while i < size do
      a(i) = List((i + 1) % size, (i + 2) % size)
      i += 1
    this.adj = a

  val operations: Map[String, () => Any] = Map(
    "bfsFull" -> { () =>
      val visited = new Array[Boolean](size)
      val queue = scala.collection.mutable.Queue(source)
      visited(source) = true
      var count = 0
      while queue.nonEmpty do
        val node = queue.dequeue()
        count += 1
        var nbrs = adj(node)
        while nbrs ne Nil do
          val nb = nbrs.head
          if !visited(nb) then
            visited(nb) = true
            queue.enqueue(nb)
          nbrs = nbrs.tail
      count
    },
  )

@main def main(): Unit = ()
