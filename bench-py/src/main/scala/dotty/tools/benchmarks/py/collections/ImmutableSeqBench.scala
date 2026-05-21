package dotty.tools.benchmarks.py.collections

/** Basic immutable sequence patterns across `List` and `Vector`: build,
 *  fold, and indexed access. */
class ImmutableSeqBench:
  var size: Int = 0
  var half: Int = 0
  var list: List[Int] = Nil
  var vec: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    half = size / 2
    list = List.range(0, size)
    vec = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "listBuild"    -> (() => List.range(0, size)),
    "listFold"     -> (() => list.foldLeft(0)(_ + _)),
    "vectorBuild"  -> (() => (0 until size).toVector),
    "vectorAccess" -> (() => vec(half)),
  )

@main def main(): Unit = ()
