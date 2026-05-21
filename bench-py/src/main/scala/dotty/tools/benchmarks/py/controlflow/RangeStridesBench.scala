package dotty.tools.benchmarks.py.controlflow

/** Strided `Range.by` iteration in `foreach`, including a descending range.
 *  Stresses how `Range.by` step variants lower compared to the stride-1 case. */
class RangeStridesBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "strideBy1" -> { () =>
      var s = 0
      (0 until size).foreach(i => s += i)
      s
    },
    "strideBy2" -> { () =>
      var s = 0
      (0 until size by 2).foreach(i => s += i)
      s
    },
    "strideBy3" -> { () =>
      var s = 0
      (0 until size by 3).foreach(i => s += i)
      s
    },
    "strideDesc" -> { () =>
      var s = 0
      (size - 1 to 0 by -1).foreach(i => s += i)
      s
    },
  )

@main def main(): Unit = ()
