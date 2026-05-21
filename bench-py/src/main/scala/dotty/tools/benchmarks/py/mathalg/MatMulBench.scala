package dotty.tools.benchmarks.py.mathalg

/** Naive 16x16 matrix multiply on flat `Array[Int]`, repeated `size`
 *  times so total work scales linearly. Four-deep nested `while`, index
 *  arithmetic (`i * N + k`) wrapping every multiply, per-element getter
 *  and setter calls on the flat arrays. */
class MatMulBench:
  private final val N = 16

  var size: Int = 0
  var a: Array[Int] = new Array[Int](0)
  var b: Array[Int] = new Array[Int](0)

  def setup(size: Int): Unit =
    this.size = size
    a = new Array[Int](N * N)
    b = new Array[Int](N * N)
    var i = 0
    while i < N * N do
      a(i) = i % 7 + 1
      b(i) = (i + 3) % 5 + 1
      i += 1

  val operations: Map[String, () => Any] = Map(
    "matmul16x16" -> { () =>
      val c = new Array[Int](N * N)
      var rep = 0
      while rep < size do
        var i = 0
        while i < N do
          var j = 0
          while j < N do
            var s = 0
            var k = 0
            while k < N do
              s += a(i * N + k) * b(k * N + j)
              k += 1
            c(i * N + j) = s
            j += 1
          i += 1
        rep += 1
      c(N * N / 2)
    },
  )

@main def main(): Unit = ()
