package dotty.tools.benchmarks.py.dispatch

/** Visitor / double-dispatch over a binary-tree node hierarchy. Each node walk
 *  pairs a bimorphic `Node.accept` dispatch with a monomorphic visitor
 *  dispatch. The generic `accept[A]` exercises a user-defined generic method on
 *  a sealed trait. */
trait NodeVisitor[A]:
  def visitLeaf(n: Leaf): A
  def visitBranch(n: Branch): A

sealed trait Node:
  def accept[A](v: NodeVisitor[A]): A

class Leaf(val value: Int) extends Node:
  def accept[A](v: NodeVisitor[A]): A = v.visitLeaf(this)

class Branch(val left: Node, val right: Node) extends Node:
  def accept[A](v: NodeVisitor[A]): A = v.visitBranch(this)

class SumVisitor extends NodeVisitor[Int]:
  def visitLeaf(n: Leaf): Int = n.value
  def visitBranch(n: Branch): Int = n.left.accept(this) + n.right.accept(this)

class CountVisitor extends NodeVisitor[Int]:
  def visitLeaf(n: Leaf): Int = 1
  def visitBranch(n: Branch): Int = n.left.accept(this) + n.right.accept(this)

class VisitorBench:
  var size: Int = 0
  var root: Node = Leaf(0)
  val sumVisitor: SumVisitor = SumVisitor()
  val countVisitor: CountVisitor = CountVisitor()

  def setup(size: Int): Unit =
    this.size = size
    root = build(size)

  def build(n: Int): Node =
    if n <= 1 then Leaf(n)
    else Branch(build(n / 2), build(n - n / 2))

  val operations: Map[String, () => Any] = Map(
    "sum"   -> (() => root.accept(sumVisitor)),
    "count" -> (() => root.accept(countVisitor)),
  )

@main def main(): Unit = ()
