package dotty.tools.benchmarks.py.pipelines

/** Prefix scans (`scanLeft`) and windowed iterators (`sliding`/`grouped`) over
 *  a `List[Int]`. Scans build an n+1 list via the standard builder; window ops
 *  yield an iterator that materializes a fresh `List` per window. */
class ScanLeftWindowBench:
  var size: Int = 0
  var data: List[Int] = Nil

  def setup(size: Int): Unit =
    this.size = size
    data = List.range(0, size)

  val operations: Map[String, () => Any] = Map(
    "scanLeftSum"  -> (() => data.scanLeft(0)(_ + _).last),
    "scanLeftProd" -> (() => data.map(_ + 1).scanLeft(1L)(_ * _).last),
    "slidingSum"   -> (() => data.sliding(8).map(_.sum).sum),
    "groupedSum"   -> (() => data.grouped(8).map(_.sum).sum),
  )

@main def main(): Unit = ()
