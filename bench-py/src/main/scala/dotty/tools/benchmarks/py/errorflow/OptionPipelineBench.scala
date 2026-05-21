package dotty.tools.benchmarks.py.errorflow

/** Monadic Option pipeline over a Vector: each op allocates Some/None wrappers
 *  and closures per element, driving Option.map/flatMap/filter/fold dispatch. */
class OptionPipelineBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (-size / 2 until size / 2).toVector

  val operations: Map[String, () => Any] = Map(
    "mapFlatMap"    -> (() => data.map(x => Some(x)).flatMap(o => o.filter(_ > 0))),
    "chainedOps"    -> (() => data.map(x => if x > 0 then Some(x * 2) else None).collect { case Some(v) => v }.sum),
    "foldGetOrElse" -> (() => data.map(x => if x % 3 == 0 then Some(x) else None).foldLeft(0)((acc, o) => acc + o.getOrElse(0))),
    "mapFold"       -> (() => data.map(x => Option(x).filter(_ % 2 == 0).fold(0)(_ + 1)).sum),
  )

@main def main(): Unit = ()
