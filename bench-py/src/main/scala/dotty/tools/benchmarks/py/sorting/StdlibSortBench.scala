package dotty.tools.benchmarks.py.sorting

/** Stdlib `.sorted` / `.sortWith` / `.sortBy` over `List` and `Vector`. These
 *  exercise `Ordering[Int]` typeclass dispatch, Int boxing, and closure
 *  allocation rather than raw array arithmetic. The structures are non-mutating
 *  inputs, re-sorted each call. */
class StdlibSortBench:
  var size: Int = 0
  var list: List[Int] = Nil
  var vec: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    list = List.range(0, size).map(i => size - i)        // descending
    vec = (0 until size).map(i => size - i).toVector

  val operations: Map[String, () => Any] = Map(
    "listSorted"   -> (() => list.sorted.head),
    "vectorSorted" -> (() => vec.sorted.head),
    "listSortWith" -> (() => list.sortWith(_ > _).head),
    "vectorSortBy" -> (() => vec.sortBy(x => -x).head),
  )

@main def main(): Unit = ()
