package dotty.tools.benchmarks.py.pipelines

/** `groupBy` / `groupMap` / `groupMapReduce` plus per-bucket aggregation over a
 *  `Vector[Int]`. Closure-heavy, builds a `HashMap`, then a second combinator
 *  pass over the values view. */
class GroupByAggregateBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "groupBySize"      -> (() => data.groupBy(_ % 8).size),
    "groupByBucketSum" -> (() => data.groupBy(_ % 8).values.map(_.sum).sum),
    "groupMapReduce"   -> (() => data.groupMapReduce(_ % 8)(identity)(_ + _).values.sum),
    "groupMap"         -> (() => data.groupMap(_ % 8)(_ * 2).values.map(_.sum).sum),
  )

@main def main(): Unit = ()
