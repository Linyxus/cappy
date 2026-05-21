package dotty.tools.benchmarks.py.controlflow

/** Early-exit linear scan expressed four ways: short-circuit `while` + `return`,
 *  non-local `return` from a `foreach` closure, and the stdlib `exists`/`find`
 *  combinators. Contrasts label-escape machinery with combinator short-circuit. */
class EarlyExitSearchBench:
  var size: Int = 0
  var data: List[Int] = Nil

  def setup(size: Int): Unit =
    this.size = size
    data = List.range(0, size)

  def firstAboveTh(th: Int): Int =
    var i = 0
    var found = -1
    val arr = data
    while i < arr.length do
      if arr(i) > th then return arr(i)
      i += 1
    found

  def findInList(th: Int): Boolean =
    data.foreach(x => if x > th then return true)
    false

  val operations: Map[String, () => Any] = Map(
    "whileEarlyExit"     -> (() => firstAboveTh(size / 2)),
    "foreachNonLocalRet" -> (() => findInList(size / 2)),
    "existsCombinator"   -> (() => data.exists(_ > size / 2)),
    "findCombinator"     -> (() => data.find(_ > size / 2).getOrElse(-1)),
  )

@main def main(): Unit = ()
