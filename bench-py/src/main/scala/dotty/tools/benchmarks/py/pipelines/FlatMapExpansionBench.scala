package dotty.tools.benchmarks.py.pipelines

/** `flatMap` fan-out over a `List[Int]`. Each call allocates a closure, a
 *  builder, and a fresh element list per input element. */
class FlatMapExpansionBench:
  var size: Int = 0
  var data: List[Int] = Nil

  def setup(size: Int): Unit =
    this.size = size
    data = List.range(0, size)

  val operations: Map[String, () => Any] = Map(
    "flatMapPair"   -> (() => data.flatMap(i => List(i, i * 2)).sum),
    "flatMapFilter" -> (() => data.flatMap(i => if i % 2 == 0 then List(i) else Nil).size),
    "flatMapNested" -> (() => data.take(32).flatMap(i => data.take(32).map(_ + i)).sum),
  )

@main def main(): Unit = ()
