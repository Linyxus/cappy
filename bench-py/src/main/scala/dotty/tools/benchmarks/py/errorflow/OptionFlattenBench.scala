package dotty.tools.benchmarks.py.errorflow

/** Option flattening and sequence-style folding: flatten over Vector[Option],
 *  monadic accumulation allocating one Some per step, and two-Option products
 *  via for-comprehension desugaring (flatMap + map). */
class OptionFlattenBench:
  var size: Int = 0
  var nested: Vector[Option[Option[Int]]] = Vector.empty
  var opts: Vector[Option[Int]] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    nested = (0 until size).toVector.map(i =>
      if i % 3 == 0 then None else Some(if i % 5 == 0 then None else Some(i)))
    opts = (0 until size).toVector.map(i => if i % 4 == 0 then None else Some(i))

  val operations: Map[String, () => Any] = Map(
    "flatten"  -> (() => nested.flatten.flatten.sum),
    "sequence" -> (() => opts.foldLeft(Some(0): Option[Int]) { (acc, o) =>
                     for a <- acc; v <- o yield a + v
                   }),
    "orElseChain" -> (() => opts.map(o => o.orElse(Some(-1))).map(_.getOrElse(0)).sum),
    "mapN" -> (() =>
      var acc = 0
      var i = 0
      while i < size - 1 do
        for a <- opts(i); b <- opts(i + 1) yield acc += a + b
        i += 1
      acc
    ),
  )

@main def main(): Unit = ()
