package dotty.tools.benchmarks.py.sorting

/** Linear-time array algorithms: counting sort (bucket histogram + write-back)
 *  and quickselect (Hoare partition, k-th smallest). Each op copies a fresh
 *  working array from `template` so every timed call processes unsorted data. */
class CountingSortBench:
  var size: Int = 0
  var template: Array[Int] = new Array[Int](0)
  val buckets: Int = 256

  def setup(size: Int): Unit =
    this.size = size
    template = new Array[Int](size)
    var i = 0
    while i < size do
      template(i) = i % buckets
      i += 1

  private def qselect(a: Array[Int], loIn: Int, hiIn: Int, k: Int): Int =
    var lo = loIn
    var hi = hiIn
    var result = -1
    while result < 0 do
      if lo == hi then result = a(lo)
      else
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
        if p == k then result = a(p)
        else if p > k then hi = p - 1
        else lo = p + 1
    result

  val operations: Map[String, () => Any] = Map(
    "countingSort" -> { () =>
      val a = new Array[Int](size)
      var k = 0
      while k < size do
        a(k) = template(k)
        k += 1
      val cnt = new Array[Int](buckets)
      k = 0
      while k < size do
        cnt(a(k)) += 1
        k += 1
      var out = 0
      var b = 0
      while b < buckets do
        var c = cnt(b)
        while c > 0 do
          a(out) = b
          out += 1
          c -= 1
        b += 1
      a(0)
    },
    "quickselect" -> { () =>
      val a = new Array[Int](size)
      var k = 0
      while k < size do
        a(k) = template(k)
        k += 1
      qselect(a, 0, size - 1, size / 2)
    },
  )

@main def main(): Unit = ()
