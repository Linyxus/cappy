package dotty.tools.benchmarks.py.text

/** String reversal two ways: building a `StringBuilder` backwards and an
 *  in-place `Array[Char]` swap followed by `new String(arr)`. */
class StringReverseBench:
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
    "builderReverse" -> { () =>
      val sb = new StringBuilder(text.length)
      var i = text.length - 1
      while i >= 0 do
        sb.append(text.charAt(i))
        i -= 1
      sb.toString
    },
    "arrayReverse" -> { () =>
      val arr = text.toCharArray()
      var lo = 0
      var hi = arr.length - 1
      while lo < hi do
        val tmp = arr(lo)
        arr(lo) = arr(hi)
        arr(hi) = tmp
        lo += 1
        hi -= 1
      new String(arr)
    },
  )

@main def main(): Unit = ()
