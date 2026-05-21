package dotty.tools.benchmarks.py.text

/** Allocation-free character scans: vowel, digit, and uppercase counting
 *  over a mixed letters/digits/spaces string. */
class CharScanBench:
  var size: Int = 0
  var text: String = ""

  def setup(size: Int): Unit =
    this.size = size
    val pattern = "abcde 12345 ABCDE "
    val sb = new StringBuilder(size)
    var i = 0
    while i < size do
      sb.append(pattern.charAt(i % pattern.length))
      i += 1
    text = sb.toString

  val operations: Map[String, () => Any] = Map(
    "vowelCount" -> { () =>
      var count = 0
      var i = 0
      while i < text.length do
        val c = text.charAt(i)
        if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' then count += 1
        i += 1
      count
    },
    "digitCount" -> { () =>
      var count = 0
      var i = 0
      while i < text.length do
        val c = text.charAt(i)
        if c >= '0' && c <= '9' then count += 1
        i += 1
      count
    },
    "upperCount" -> { () =>
      var count = 0
      var i = 0
      while i < text.length do
        val c = text.charAt(i)
        if c >= 'A' && c <= 'Z' then count += 1
        i += 1
      count
    },
  )

@main def main(): Unit = ()
