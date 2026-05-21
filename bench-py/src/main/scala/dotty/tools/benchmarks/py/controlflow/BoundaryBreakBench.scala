package dotty.tools.benchmarks.py.controlflow

import scala.util.boundary, boundary.break

/** Early-exit search via `scala.util.boundary`/`break`, contrasted with a
 *  short-circuit `while` + `return` and the `exists` combinator. All three go
 *  through (or bypass) the labeled-block raise/catch path differently. */
class BoundaryBreakBench:
  var size: Int = 0
  var data: List[Int] = Nil

  def setup(size: Int): Unit =
    this.size = size
    data = List.range(0, size)

  def searchBoundary(th: Int): Int =
    boundary:
      data.foreach(x => if x >= th then break(x))
      -1

  def searchWhile(th: Int): Int =
    var i = 0
    val arr = data
    while i < arr.length do
      if arr(i) >= th then return arr(i)
      i += 1
    -1

  val operations: Map[String, () => Any] = Map(
    "boundaryBreakEarlyExit" -> (() => searchBoundary(size / 2)),
    "whileEarlyExit"         -> (() => searchWhile(size / 2)),
    "existsEarlyExit"        -> (() => data.exists(_ >= size / 2)),
  )

@main def main(): Unit = ()
