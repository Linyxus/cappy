package dotty.tools.benchmarks.py.pipelines

/** `partition`, `sorted`/`sortWith`, and top-k `take` over a `List[Int]` with a
 *  deterministic pseudo-shuffled order. Exercises predicate closures, two-list
 *  partition builders, and `Ordering[Int]` virtual dispatch. */
class PartitionSortTopKBench:
  var size: Int = 0
  var data: List[Int] = Nil

  def setup(size: Int): Unit =
    this.size = size
    data = List.tabulate(size)(i => (i * 1664525 + 1013904223) & 0x7FFFFFFF)

  val operations: Map[String, () => Any] = Map(
    "partition"    -> (() => { val (lo, hi) = data.partition(_ < Int.MaxValue / 2); lo.size + hi.size }),
    "sortTake"     -> (() => data.sorted.take(10).sum),
    "sortWithTake" -> (() => data.sortWith(_ > _).take(10).sum),
    "partSortSum"  -> (() => { val (lo, _) = data.partition(_ < Int.MaxValue / 2); lo.sorted.take(10).sum }),
  )

@main def main(): Unit = ()
