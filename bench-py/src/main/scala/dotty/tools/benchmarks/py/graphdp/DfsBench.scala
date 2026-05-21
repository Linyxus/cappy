package dotty.tools.benchmarks.py.graphdp

/** Iterative depth-first traversal with an explicit `ArrayBuffer` stack
 *  over a sparse adjacency list. */
class DfsBench:
  var size: Int = 0
  var adj: Array[List[Int]] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val a = new Array[List[Int]](size)
    var i = 0
    while i < size do
      a(i) = List((i + 1) % size, (i + 2) % size)
      i += 1
    this.adj = a

  val operations: Map[String, () => Any] = Map(
    "dfsIterative" -> { () =>
      val visited = new Array[Boolean](size)
      val stack = scala.collection.mutable.ArrayBuffer[Int]()
      stack += 0
      var count = 0
      while stack.nonEmpty do
        val node = stack.remove(stack.size - 1)
        if !visited(node) then
          visited(node) = true
          count += 1
          var nbrs = adj(node)
          while nbrs ne Nil do
            if !visited(nbrs.head) then stack += nbrs.head
            nbrs = nbrs.tail
      count
    },
  )

@main def main(): Unit = ()
