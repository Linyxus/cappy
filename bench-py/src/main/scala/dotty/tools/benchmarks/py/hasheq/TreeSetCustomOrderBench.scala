package dotty.tools.benchmarks.py.hasheq

import scala.collection.immutable.TreeSet

/** `TreeSet` insertion + range queries under a custom `given Ordering`. Each
 *  tree comparison goes through Ordering dispatch + `Double.compare`. */
case class Scored(key: Int, score: Double)

given Ordering[Scored] with
  def compare(a: Scored, b: Scored): Int =
    val c = java.lang.Double.compare(a.score, b.score)
    if c != 0 then -c // descending score
    else Integer.compare(a.key, b.key)

class TreeSetCustomOrderBench:
  var size: Int = 0
  var items: Vector[Scored] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    items = (0 until size).map(i => Scored(i, (i * 0.1) % 13.7)).toVector

  val operations: Map[String, () => Any] = Map(
    "buildTreeSet" -> (() => TreeSet.from(items)),
    "insertions"   -> (() => items.foldLeft(TreeSet.empty[Scored])(_ + _)),
    "headTail"     -> (() => {
      val ts = TreeSet.from(items)
      (ts.head, ts.last)
    }),
    "rangeQuery" -> (() => {
      val ts = TreeSet.from(items)
      ts.rangeFrom(Scored(size / 4, 0.0)).size
    }),
  )

@main def main(): Unit = ()
