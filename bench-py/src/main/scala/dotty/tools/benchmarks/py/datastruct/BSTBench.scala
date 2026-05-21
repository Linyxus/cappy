package dotty.tools.benchmarks.py.datastruct

/** Unbalanced binary search tree over Int keys, built by functional insert.
 *  Allocation per insert, pattern-match dispatch and pointer-chasing per
 *  node visit, deep recursion on lookup/depth/traversal. */
sealed trait BST
case object BSTLeaf extends BST
case class BSTNode(key: Int, left: BST, right: BST) extends BST

class BSTBench:
  var size: Int = 0
  var tree: BST = BSTLeaf

  def insert(t: BST, k: Int): BST = t match
    case BSTLeaf => BSTNode(k, BSTLeaf, BSTLeaf)
    case node @ BSTNode(key, l, r) =>
      if k < key then BSTNode(key, insert(l, k), r)
      else if k > key then BSTNode(key, l, insert(r, k))
      else node

  def contains(t: BST, k: Int): Boolean = t match
    case BSTLeaf => false
    case BSTNode(key, l, r) =>
      if k < key then contains(l, k)
      else if k > key then contains(r, k)
      else true

  def depth(t: BST): Int = t match
    case BSTLeaf            => 0
    case BSTNode(_, l, r)   =>
      val dl = depth(l)
      val dr = depth(r)
      1 + (if dl >= dr then dl else dr)

  def inOrderSum(t: BST): Long = t match
    case BSTLeaf            => 0L
    case BSTNode(key, l, r) => inOrderSum(l) + key.toLong + inOrderSum(r)

  def keyAt(i: Int): Int = ((i * 2654435761L) & 0x7fffffffL).toInt % (size * 4 + 1)

  def build(n: Int): BST =
    var t: BST = BSTLeaf
    var i = 0
    while i < n do
      t = insert(t, keyAt(i))
      i += 1
    t

  def setup(size: Int): Unit =
    this.size = size
    tree = build(size)

  val operations: Map[String, () => Any] = Map(
    "lookup" -> { () =>
      var found = 0
      var i = 0
      while i < size do
        if contains(tree, keyAt(i)) then found += 1
        i += 1
      found
    },
    "insertFresh" -> (() => build(size)),
    "depth"       -> (() => depth(tree)),
    "inOrderSum"  -> (() => inOrderSum(tree)),
  )

@main def main(): Unit = ()
