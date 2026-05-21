package dotty.tools.benchmarks.py.sorting

/** Sorting case-class records by an Int key. Stresses case-class field access
 *  through virtual dispatch on every comparison, plus `Ordering` typeclass
 *  boxing and closure capture. Inputs are non-mutating, re-sorted each call. */
case class Record(key: Int, secondary: Int)

given Ordering[Record] with
  def compare(a: Record, b: Record): Int =
    if a.key < b.key then -1
    else if a.key > b.key then 1
    else 0

class RecordSortBench:
  var size: Int = 0
  var records: Vector[Record] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    records = (0 until size).map(i => Record(size - i, i % 7)).toVector

  val operations: Map[String, () => Any] = Map(
    "sortByKey"       -> (() => records.sortBy(_.key).head.key),
    "sortBySecondary" -> (() => records.sortBy(_.secondary).head.secondary),
    "sortedCustom"    -> (() => records.sorted.head.key),
  )

@main def main(): Unit = ()
