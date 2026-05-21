package dotty.tools.benchmarks.py.text

import scala.collection.mutable.HashMap

/** Character-frequency histograms built two ways: a flat `Array[Int]`
 *  bucket table and a `mutable.HashMap[Char, Int]`. */
class CharFreqBench:
  var size: Int = 0
  var text: String = ""

  def setup(size: Int): Unit =
    this.size = size
    val sb = new StringBuilder(size)
    var i = 0
    while i < size do
      sb.append(('a' + (i % 26)).toChar)
      i += 1
    text = sb.toString

  val operations: Map[String, () => Any] = Map(
    "charFreqArray" -> { () =>
      val counts = new Array[Int](128)
      var i = 0
      while i < text.length do
        counts(text.charAt(i)) += 1
        i += 1
      var nonZero = 0
      var j = 0
      while j < 128 do
        if counts(j) > 0 then nonZero += 1
        j += 1
      nonZero
    },
    "charFreqMap" -> { () =>
      val m = HashMap.empty[Char, Int]
      var i = 0
      while i < text.length do
        val c = text.charAt(i)
        m(c) = m.getOrElse(c, 0) + 1
        i += 1
      m.size
    },
  )

@main def main(): Unit = ()
