package dotty.tools.benchmarks.py.errorflow

import scala.util.{Try, Success, Failure}

/** Try-wrapped parsing of generated strings: `Try { }` wraps a try/catch per
 *  element, exercising exception throw+catch machinery for the "bad" inputs,
 *  plus Success/Failure allocation and closures in recover/map. */
class TryParseBench:
  var size: Int = 0
  var inputs: Vector[String] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    inputs = (0 until size).toVector.map(i => if i % 3 == 0 then "bad" else i.toString)

  def parse(s: String): Try[Int] = Try(s.toInt)

  val operations: Map[String, () => Any] = Map(
    "tryParse"     -> (() => inputs.map(parse).count(_.isSuccess)),
    "tryMapFlat"   -> (() => inputs.map(parse).map(_.map(_ * 2)).count(_.isSuccess)),
    "tryGetOrElse" -> (() => inputs.map(parse).map(_.getOrElse(-1)).sum),
    "tryRecover"   -> (() => inputs.map(parse).map(_.recover { case _: NumberFormatException => -1 }).map(_.get).sum),
  )

@main def main(): Unit = ()
