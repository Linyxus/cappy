package dotty.tools.benchmarks.py.hasheq

/** Hand-rolled `h = h * 31 + x` accumulator — raw `_scpy_i32` truncation
 *  pressure — paired with stdlib `.hashCode` variants for comparison. */
case class Pair(a: Int, b: Int)

class HandRolledHashBench:
  var size: Int = 0
  var data: Array[Int] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    data = (0 until size).map(i => i * 1664525 + 1013904223).toArray

  val operations: Map[String, () => Any] = Map(
    "manualMurmur" -> (() => {
      var h = -1640531527
      var i = 0
      while i < size do
        h = h * 31 + data(i)
        i += 1
      h
    }),
    "stdlibHashInt" -> (() => {
      var s = 0
      var i = 0
      while i < size do
        s += (data(i): Any).##
        i += 1
      s
    }),
    "pairHashSum" -> (() => {
      var s = 0
      var i = 0
      while i < size do
        s += Pair(data(i), data(i) ^ 0xDEADBEEF).hashCode
        i += 1
      s
    }),
  )

@main def main(): Unit = ()
