package dotty.tools.benchmarks.py.mathalg

/** Flat-array integer kernels: dot product, prefix sum, running max.
 *  Array reads per iteration through the `_scpy_Array` getter, `size`
 *  read every iteration in the loop bound, `toLong` widening into the
 *  64-bit domain. */
class ArrayArithBench:
  var size: Int = 0
  var xs: Array[Int] = new Array[Int](0)
  var ys: Array[Int] = new Array[Int](0)

  def setup(size: Int): Unit =
    this.size = size
    xs = new Array[Int](size)
    ys = new Array[Int](size)
    var i = 0
    while i < size do
      xs(i) = i % 97 + 1
      ys(i) = (i * 3 + 7) % 101 + 1
      i += 1

  val operations: Map[String, () => Any] = Map(
    "dotProduct" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += xs(i).toLong * ys(i)
        i += 1
      s
    },
    "prefixSum" -> { () =>
      val out = new Array[Int](size)
      out(0) = xs(0)
      var i = 1
      while i < size do
        out(i) = out(i - 1) + xs(i)
        i += 1
      out(size - 1)
    },
    "maxScan" -> { () =>
      var m = xs(0)
      var i = 1
      while i < size do
        if xs(i) > m then m = xs(i)
        i += 1
      m
    },
  )

@main def main(): Unit = ()
