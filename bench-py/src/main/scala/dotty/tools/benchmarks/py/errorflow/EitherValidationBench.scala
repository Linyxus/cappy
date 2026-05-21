package dotty.tools.benchmarks.py.errorflow

/** Either-based validation pipeline: Right/Left wrapper allocation per element,
 *  Function1 closures in flatMap/fold/map, and right-biased dispatch overhead. */
class EitherValidationBench:
  var size: Int = 0
  var inputs: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    inputs = (0 until size).toVector

  def validate(x: Int): Either[String, Int] =
    if x % 2 == 0 then Right(x * 3)
    else Left(s"bad:$x")

  def chain(x: Int): Either[String, Int] =
    validate(x).flatMap(v => if v > size then Right(v) else Left(s"small:$v"))

  val operations: Map[String, () => Any] = Map(
    "mapFlatMap"    -> (() => inputs.map(validate).count(_.isRight)),
    "chainedFlat"   -> (() => inputs.map(chain).count(_.isRight)),
    "foldBoth"      -> (() => inputs.map(validate).foldLeft((0, 0)) { case ((l, r), e) =>
                          if e.isRight then (l, r + 1) else (l + 1, r)
                        }),
    "getOrElseFold" -> (() => inputs.map(v => validate(v).fold(_ => -1, identity)).sum),
  )

@main def main(): Unit = ()
