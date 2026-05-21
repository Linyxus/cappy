package dotty.tools.benchmarks.py.closures

/** Higher-order combinators over a `Vector[Int]`: each op allocates a lambda
 *  and drives Function1/Function2 dispatch through the collection library. */
class HigherOrderBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "mapInc"        -> (() => data.map(_ + 1)),
    "filterEven"    -> (() => data.filter(_ % 2 == 0)),
    "foldSum"       -> (() => data.foldLeft(0)(_ + _)),
    "mapFilterFold" -> (() => data.map(_ * 2).filter(_ > size).foldLeft(0L)(_ + _)),
  )

@main def main(): Unit = ()
