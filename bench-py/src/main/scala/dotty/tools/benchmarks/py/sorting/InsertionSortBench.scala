package dotty.tools.benchmarks.py.sorting

/** In-place insertion sort over `Array[Int]`. Worst case (reverse-sorted)
 *  input: every element shifts all the way left, so the inner loop runs the
 *  full O(n^2) comparison/assignment traffic. Each op copies a fresh working
 *  array from `template` so every timed call sorts unsorted data. */
class InsertionSortBench:
  var size: Int = 0
  var template: Array[Int] = new Array[Int](0)

  def setup(size: Int): Unit =
    this.size = size
    template = new Array[Int](size)
    var i = 0
    while i < size do
      template(i) = size - i   // descending => worst case
      i += 1

  val operations: Map[String, () => Any] = Map(
    "insertionSort" -> { () =>
      val a = new Array[Int](size)
      var k = 0
      while k < size do
        a(k) = template(k)
        k += 1
      var i = 1
      while i < size do
        val key = a(i)
        var j = i - 1
        while j >= 0 && a(j) > key do
          a(j + 1) = a(j)
          j -= 1
        a(j + 1) = key
        i += 1
      a(0)
    },
  )

@main def main(): Unit = ()
