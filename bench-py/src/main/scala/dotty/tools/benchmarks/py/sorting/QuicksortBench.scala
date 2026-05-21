package dotty.tools.benchmarks.py.sorting

/** Recursive in-place quicksort (Lomuto partition) over `Array[Int]`. Each op
 *  copies a fresh working array from `template` so every timed call sorts
 *  unsorted data. */
class QuicksortBench:
  var size: Int = 0
  var template: Array[Int] = new Array[Int](0)

  def setup(size: Int): Unit =
    this.size = size
    template = new Array[Int](size)
    var i = 0
    while i < size do
      // deterministic scrambled pattern in [0, size)
      template(i) = ((i * 2654435761L) & 0x7fffffffL).toInt % size
      i += 1

  private def qsort(a: Array[Int], lo: Int, hi: Int): Unit =
    if lo < hi then
      val pivot = a(hi)
      var i = lo - 1
      var j = lo
      while j < hi do
        if a(j) <= pivot then
          i += 1
          val t = a(i); a(i) = a(j); a(j) = t
        j += 1
      val t = a(i + 1); a(i + 1) = a(hi); a(hi) = t
      val p = i + 1
      qsort(a, lo, p - 1)
      qsort(a, p + 1, hi)

  val operations: Map[String, () => Any] = Map(
    "quicksort" -> { () =>
      val a = new Array[Int](size)
      var k = 0
      while k < size do
        a(k) = template(k)
        k += 1
      qsort(a, 0, size - 1)
      a(0)
    },
  )

@main def main(): Unit = ()
