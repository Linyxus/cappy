package dotty.tools.benchmarks.py.errorflow

import scala.util.boundary, boundary.break

/** Contrasts scala.util.boundary/break (labeled non-local return, lowered via
 *  raise/try/except in Python) against Option.find (allocates Some at the match
 *  site), on both early-exit and exhausting traversal paths. */
class BoundaryVsOptionBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  def firstNegBoundary(xs: Vector[Int]): Option[Int] =
    boundary:
      xs.foreach(x => if x < 0 then break(Some(x)))
      None

  def firstNegOption(xs: Vector[Int]): Option[Int] =
    xs.find(_ < 0)

  val operations: Map[String, () => Any] = Map(
    "boundaryEarlyExit" -> (() => firstNegBoundary(data.map(_ - size / 3))),
    "optionFind"        -> (() => firstNegOption(data.map(_ - size / 3))),
    "boundaryNoExit"    -> (() => firstNegBoundary(data)),
    "optionFindMiss"    -> (() => firstNegOption(data)),
  )

@main def main(): Unit = ()
