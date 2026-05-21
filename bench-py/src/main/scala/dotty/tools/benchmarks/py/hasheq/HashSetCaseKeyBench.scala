package dotty.tools.benchmarks.py.hasheq

import scala.collection.immutable.HashSet

/** A `HashSet` keyed by case-class instances: hit/miss membership probes drive
 *  case-class `.hashCode` + `equals` through the trie lookup path. */
case class Record(id: Int, tag: String)

class HashSetCaseKeyBench:
  var size: Int = 0
  var set: HashSet[Record] = HashSet.empty
  var probes: Vector[Record] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    set = HashSet.from((0 until size).map(i => Record(i, s"tag${i % 8}")))
    probes = (0 until size).map(i => Record(i, s"tag${i % 8}")).toVector

  val operations: Map[String, () => Any] = Map(
    "buildSet" -> (() =>
      HashSet.from((0 until size).map(i => Record(i, s"tag${i % 8}")))
    ),
    "containsHit" -> (() => {
      var n = 0
      var i = 0
      while i < probes.size do
        if set.contains(probes(i)) then n += 1
        i += 1
      n
    }),
    "containsMiss" -> (() => {
      var n = 0
      var i = 0
      while i < probes.size do
        if set.contains(Record(i + size, "miss")) then n += 1
        i += 1
      n
    }),
  )

@main def main(): Unit = ()
