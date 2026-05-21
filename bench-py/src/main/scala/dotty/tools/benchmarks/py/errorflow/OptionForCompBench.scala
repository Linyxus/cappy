package dotty.tools.benchmarks.py.errorflow

/** For-comprehension desugaring: `yield` over collections plus for-comprehensions
 *  over Option with guards, exercising Option.flatMap/map/withFilter chains and
 *  short-lived Some/None allocation with captured closures. */
class OptionForCompBench:
  var size: Int = 0
  var xs: Vector[Int] = Vector.empty
  var ys: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    xs = (1 to size).toVector
    ys = (1 to size by 2).toVector

  val operations: Map[String, () => Any] = Map(
    "forYield" -> (() =>
      (for
        x <- xs
        y <- ys
        if x + y < size
      yield x + y).sum
    ),
    "optionForComp" -> (() =>
      var acc = 0
      for i <- 0 until size do
        val result =
          for
            a <- if i % 2 == 0 then Some(i) else None
            b <- if i % 3 != 0 then Some(i * 2) else None
          yield a + b
        acc += result.getOrElse(0)
      acc
    ),
  )

@main def main(): Unit = ()
