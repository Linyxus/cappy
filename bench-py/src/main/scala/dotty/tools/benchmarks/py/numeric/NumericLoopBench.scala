package dotty.tools.benchmarks.py.numeric

/** Tight integer hot loops: the most basic codegen quality signal.
 *  No allocation, no stdlib calls — just `while` + arithmetic. */
class NumericLoopBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "sumLoop" -> { () =>
      var s = 0
      var i = 0
      while i < size do
        s += i
        i += 1
      s
    },
    "mulAccum" -> { () =>
      var s = 1L
      var i = 1
      while i <= size do
        s = s * 31 + i
        i += 1
      s
    },
    "divmod" -> { () =>
      var s = 0
      var i = 1
      while i <= size do
        s += (i * 7) % 13 + i / 3
        i += 1
      s
    },
  )

@main def main(): Unit = ()
