package dotty.tools.benchmarks.py.controlflow

/** Same integer sum expressed four ways: hand-written `while`, `Range.foreach`,
 *  `Range.foldLeft`, and `Range.sum`. Gives a future foreach-to-while lowering
 *  pass a direct comparison point. */
class WhileVsForeachSumBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "whileLoop" -> { () =>
      var s = 0
      var i = 0
      while i < size do
        s += i
        i += 1
      s
    },
    "rangeForEach" -> { () =>
      var s = 0
      (0 until size).foreach(i => s += i)
      s
    },
    "rangeFoldLeft" -> (() => (0 until size).foldLeft(0)(_ + _)),
    "rangeSum"      -> (() => (0 until size).sum),
  )

@main def main(): Unit = ()
