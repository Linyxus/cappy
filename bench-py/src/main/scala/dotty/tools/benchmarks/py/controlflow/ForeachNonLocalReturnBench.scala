package dotty.tools.benchmarks.py.controlflow

/** Non-local `return` from a `foreach` closure, isolated and contrasted with an
 *  index `while` scan and the `exists` combinator. The early-hit op measures the
 *  fixed cost of the label raise/catch machinery without loop-body work. */
class ForeachNonLocalReturnBench:
  var size: Int = 0
  var data: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).toVector

  def findSentinel(target: Int): Int =
    data.foreach(x => if x == target then return x)
    -1

  def whileFindSentinel(target: Int): Int =
    var i = 0
    while i < data.length do
      if data(i) == target then return data(i)
      i += 1
    -1

  val operations: Map[String, () => Any] = Map(
    "nonLocalRetForeach"  -> (() => findSentinel(size - 1)),
    "nonLocalRetEarlyHit" -> (() => findSentinel(0)),
    "whileIndexScan"      -> (() => whileFindSentinel(size - 1)),
    "existsScan"          -> (() => data.exists(_ == size - 1)),
  )

@main def main(): Unit = ()
