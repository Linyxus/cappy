package dotty.tools.benchmarks.py.text

/** Run-length encode/decode over a string composed of fixed-length runs.
 *  Stresses nested scan loops, integer accumulation, and per-char append. */
class RunLengthBench:
  var size: Int = 0
  var plain: String = ""
  var encoded: String = ""

  def setup(size: Int): Unit =
    this.size = size
    val letters = "abcdef"
    val sb = new StringBuilder(size)
    var i = 0
    while i < size do
      val letter = letters.charAt((i / 8) % letters.length)
      sb.append(letter)
      i += 1
    plain = sb.toString
    encoded = encode(plain)

  private def encode(s: String): String =
    val sb = new StringBuilder()
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      var count = 0
      while i < s.length && s.charAt(i) == c do
        count += 1
        i += 1
      sb.append(count)
      sb.append(c)
    sb.toString

  val operations: Map[String, () => Any] = Map(
    "encode" -> (() => encode(plain)),
    "decode" -> { () =>
      val sb = new StringBuilder()
      var i = 0
      while i < encoded.length do
        var count = 0
        while i < encoded.length && encoded.charAt(i) >= '0' && encoded.charAt(i) <= '9' do
          count = count * 10 + (encoded.charAt(i) - '0')
          i += 1
        val c = encoded.charAt(i)
        i += 1
        var j = 0
        while j < count do
          sb.append(c)
          j += 1
      sb.toString
    },
  )

@main def main(): Unit = ()
