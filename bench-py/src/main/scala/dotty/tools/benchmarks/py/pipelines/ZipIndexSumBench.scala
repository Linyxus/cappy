package dotty.tools.benchmarks.py.pipelines

/** `zip` / `zipWithIndex` followed by a paired `map`/`foldLeft`. Each step
 *  allocates a `(Int, Int)` tuple per element that the downstream stage must
 *  unbox via `_1`/`_2`. */
class ZipIndexSumBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty
  var data2: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector
    data2 = (size until size * 2).toVector

  val operations: Map[String, () => Any] = Map(
    "zipWithIndex" -> (() => data.zipWithIndex.map((v, i) => v + i).sum),
    "zipTwoVecs"   -> (() => data.zip(data2).map((a, b) => a * b).sum),
    "zipFoldLeft"  -> (() => data.zip(data2).foldLeft(0L)((acc, p) => acc + p._1 + p._2)),
  )

@main def main(): Unit = ()
