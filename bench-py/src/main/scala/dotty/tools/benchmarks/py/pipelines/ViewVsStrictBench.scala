package dotty.tools.benchmarks.py.pipelines

/** Contrasts strict map/filter chains (intermediate `Vector` allocation) with
 *  lazy `.view` chains (`SeqView`/`IndexedSeqView` wrapper overhead). */
class ViewVsStrictBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "strictMapFilter" -> (() => data.map(_ * 2).filter(_ > size).foldLeft(0L)(_ + _)),
    "viewMapFilter"   -> (() => data.view.map(_ * 2).filter(_ > size).foldLeft(0L)(_ + _)),
    "viewChain3"      -> (() => data.view.map(_ + 1).filter(_ % 3 != 0).map(_ * 7).foldLeft(0L)(_ + _)),
    "strictChain3"    -> (() => data.map(_ + 1).filter(_ % 3 != 0).map(_ * 7).foldLeft(0L)(_ + _)),
  )

@main def main(): Unit = ()
