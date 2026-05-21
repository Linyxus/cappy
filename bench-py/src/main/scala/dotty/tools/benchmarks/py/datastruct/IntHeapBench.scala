package dotty.tools.benchmarks.py.datastruct

/** Binary min-heap over a 1-indexed mutable Array[Int]. No case classes;
 *  isolates Array index read/write and Int arithmetic codegen overhead. */
class IntHeapBench:
  var size: Int = 0
  var data: Array[Int] = new Array[Int](1)

  def heapifyDown(arr: Array[Int], n: Int, start: Int): Unit =
    var pos = start
    var go = true
    while go do
      val l = pos * 2
      val r = l + 1
      var smallest = pos
      if l <= n && arr(l) < arr(smallest) then smallest = l
      if r <= n && arr(r) < arr(smallest) then smallest = r
      if smallest != pos then
        val tmp = arr(pos); arr(pos) = arr(smallest); arr(smallest) = tmp
        pos = smallest
      else go = false

  def heapifyUp(arr: Array[Int], start: Int): Unit =
    var pos = start
    while pos > 1 && arr(pos / 2) > arr(pos) do
      val parent = pos / 2
      val tmp = arr(parent); arr(parent) = arr(pos); arr(pos) = tmp
      pos = parent

  def buildHeap(n: Int): Array[Int] =
    val arr = new Array[Int](n + 1)
    var i = 1
    var x = 42
    while i <= n do
      x = x * 1664525 + 1013904223
      arr(i) = x & 0x7fffffff
      i += 1
    var j = n / 2
    while j >= 1 do
      heapifyDown(arr, n, j)
      j -= 1
    arr

  def setup(size: Int): Unit =
    this.size = size
    data = buildHeap(size)

  val operations: Map[String, () => Any] = Map(
    "heapSort" -> { () =>
      val arr = new Array[Int](size + 1)
      var i = 0
      while i <= size do
        arr(i) = data(i)
        i += 1
      var n = size
      var last = 0
      while n > 1 do
        last = arr(1)
        arr(1) = arr(n)
        n -= 1
        heapifyDown(arr, n, 1)
      last
    },
    "pushPop" -> { () =>
      val cap = size + 1
      val arr = new Array[Int](cap + 1)
      var count = 0
      var x = 7
      val half = size / 2
      var i = 0
      while i < half do
        x = x * 1664525 + 1013904223
        count += 1
        arr(count) = x & 0x7fffffff
        heapifyUp(arr, count)
        i += 1
      var last = 0
      while count > 0 do
        last = arr(1)
        arr(1) = arr(count)
        count -= 1
        heapifyDown(arr, count, 1)
      last
    },
    "minPeek" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += data(1).toLong
        i += 1
      s
    },
  )

@main def main(): Unit = ()
