package dotty.tools.benchmarks.py.datastruct

/** Functional leftist min-heap built from case-class nodes. Extreme
 *  allocation pressure (O(log n) nodes per insert/deleteMin), deep mutual
 *  recursion in merge, and tuple-of-ADT pattern-match dispatch. */
sealed trait LHeap
case object LEmpty extends LHeap
case class LNode(rank: Int, key: Int, left: LHeap, right: LHeap) extends LHeap

class HeapTreeBench:
  var size: Int = 0
  var heap: LHeap = LEmpty

  def rankOf(h: LHeap): Int = h match
    case LEmpty            => 0
    case LNode(r, _, _, _) => r

  def makeNode(k: Int, a: LHeap, b: LHeap): LNode =
    if rankOf(a) >= rankOf(b) then LNode(rankOf(b) + 1, k, a, b)
    else LNode(rankOf(a) + 1, k, b, a)

  def merge(x: LHeap, y: LHeap): LHeap = (x, y) match
    case (LEmpty, _) => y
    case (_, LEmpty) => x
    case (LNode(_, kx, lx, rx), LNode(_, ky, ly, ry)) =>
      if kx <= ky then makeNode(kx, lx, merge(rx, y))
      else makeNode(ky, ly, merge(x, ry))

  def insert(h: LHeap, k: Int): LHeap = merge(LNode(1, k, LEmpty, LEmpty), h)

  def findMin(h: LHeap): Int = h match
    case LNode(_, k, _, _) => k
    case LEmpty            => Int.MaxValue

  def deleteMin(h: LHeap): LHeap = h match
    case LNode(_, _, l, r) => merge(l, r)
    case LEmpty            => LEmpty

  def build(n: Int): LHeap =
    var h: LHeap = LEmpty
    var i = 0
    var x = 1
    while i < n do
      x = x * 1664525 + 1013904223
      h = insert(h, x & 0x7fffffff)
      i += 1
    h

  def setup(size: Int): Unit =
    this.size = size
    heap = build(size)

  val operations: Map[String, () => Any] = Map(
    "insertBatch" -> (() => findMin(build(size))),
    "deleteMinSeq" -> { () =>
      var h: LHeap = heap
      var s = 0L
      var go = true
      while go do
        h match
          case LEmpty => go = false
          case _      => s += findMin(h).toLong; h = deleteMin(h)
      s
    },
    "mergeTrees" -> { () =>
      val half = size / 2
      val a = build(half)
      val b = build(size - half)
      findMin(merge(a, b))
    },
  )

@main def main(): Unit = ()
