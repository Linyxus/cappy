package dotty.tools.benchmarks.py.hasheq

import scala.collection.immutable.HashMap

/** A `HashMap` keyed by case-class instances: build, lookup, and update fire
 *  case-class `.hashCode` + `equals` on every trie probe. */
case class Cell(row: Int, col: Int)

class HashMapCaseKeyBench:
  var size: Int = 0
  var map: HashMap[Cell, Int] = HashMap.empty
  var keys: Vector[Cell] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    var n = 1
    while n * n < size do n += 1
    keys = (0 until size).map(i => Cell(i / n, i % n)).toVector
    map = HashMap.from(keys.zipWithIndex)

  val operations: Map[String, () => Any] = Map(
    "buildMap"  -> (() => HashMap.from(keys.zipWithIndex)),
    "lookupAll" -> (() => {
      var s = 0
      keys.foreach(k => s += map.getOrElse(k, 0))
      s
    }),
    "updateAll" -> (() =>
      keys.foldLeft(map)((m, k) => m.updated(k, m.getOrElse(k, 0) + 1))
    ),
  )

@main def main(): Unit = ()
