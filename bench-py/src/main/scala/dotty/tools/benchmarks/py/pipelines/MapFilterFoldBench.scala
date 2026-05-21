package dotty.tools.benchmarks.py.pipelines

/** Strict map/filter/fold chains of increasing length over a `Vector[Int]`.
 *  Each combinator stage allocates a lambda, drives Function1 dispatch per
 *  element, and materializes an intermediate collection. */
class MapFilterFoldBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "map1"          -> (() => data.map(_ * 2).sum),
    "mapFilter"     -> (() => data.map(_ * 2).filter(_ > size).size),
    "mapFilterFold" -> (() => data.map(_ * 2).filter(_ > size).foldLeft(0L)(_ + _)),
    "chain4"        -> (() => data.map(_ + 1).filter(_ % 3 != 0).map(_ * 7).foldLeft(0L)(_ + _)),
  )

@main def main(): Unit = ()
