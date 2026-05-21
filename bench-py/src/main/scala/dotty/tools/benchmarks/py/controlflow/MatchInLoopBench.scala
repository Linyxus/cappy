package dotty.tools.benchmarks.py.controlflow

/** A pattern match inside a tight loop, contrasted with an equivalent if-chain
 *  and with the match driven by `foreach`. Exercises per-iteration labeled-block
 *  hoisting (the label class must be declared once per call, not per iteration). */
class MatchInLoopBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "matchInWhile" -> { () =>
      var s = 0
      var i = 0
      while i < size do
        s += ((i % 4) match
          case 0 => 1
          case 1 => 2
          case 2 => 3
          case _ => 4)
        i += 1
      s
    },
    "ifChainInWhile" -> { () =>
      var s = 0
      var i = 0
      while i < size do
        val m = i % 4
        s += (if m == 0 then 1 else if m == 1 then 2 else if m == 2 then 3 else 4)
        i += 1
      s
    },
    "matchInForeach" -> { () =>
      var s = 0
      (0 until size).foreach: i =>
        s += ((i % 4) match
          case 0 => 1
          case 1 => 2
          case 2 => 3
          case _ => 4)
      s
    },
  )

@main def main(): Unit = ()
