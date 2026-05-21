package dotty.tools.benchmarks.py.sorting

/** Top-down merge sort over `Array[Int]`, allocating a temporary buffer per
 *  merge. Each op copies a fresh working array from `template` so every timed
 *  call sorts unsorted data. */
class MergeSortBench:
  var size: Int = 0
  var template: Array[Int] = new Array[Int](0)

  def setup(size: Int): Unit =
    this.size = size
    template = new Array[Int](size)
    var i = 0
    while i < size do
      template(i) = size - i   // descending
      i += 1

  /** Sort a[lo, hi) in place. */
  private def msort(a: Array[Int], lo: Int, hi: Int): Unit =
    if hi - lo > 1 then
      val mid = lo + (hi - lo) / 2
      msort(a, lo, mid)
      msort(a, mid, hi)
      val tmp = new Array[Int](hi - lo)
      var i = lo
      var j = mid
      var t = 0
      while i < mid && j < hi do
        if a(i) <= a(j) then { tmp(t) = a(i); i += 1 }
        else { tmp(t) = a(j); j += 1 }
        t += 1
      while i < mid do { tmp(t) = a(i); i += 1; t += 1 }
      while j < hi do { tmp(t) = a(j); j += 1; t += 1 }
      var k = 0
      while k < tmp.length do
        a(lo + k) = tmp(k)
        k += 1

  val operations: Map[String, () => Any] = Map(
    "mergeSort" -> { () =>
      val a = new Array[Int](size)
      var k = 0
      while k < size do
        a(k) = template(k)
        k += 1
      msort(a, 0, size)
      a(0)
    },
  )

@main def main(): Unit = ()
