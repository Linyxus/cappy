package dotty.tools.benchmarks.py.text

/** Substring search via a hand-rolled `charAt` double loop versus the
 *  javalib `String.indexOf(String, fromIndex)` shim. */
class NaiveSubstringSearchBench:
  var size: Int = 0
  var haystack: String = ""
  var needle: String = ""

  def setup(size: Int): Unit =
    this.size = size
    val pattern = "abcde"
    val sb = new StringBuilder(size)
    var i = 0
    while i < size do
      sb.append(pattern.charAt(i % pattern.length))
      i += 1
    haystack = sb.toString
    needle = pattern

  val operations: Map[String, () => Any] = Map(
    "naiveSearch" -> { () =>
      val n = needle.length
      val h = haystack.length
      var pos = 0
      var found = 0
      while pos <= h - n do
        var j = 0
        var ok = true
        while j < n && ok do
          if haystack.charAt(pos + j) != needle.charAt(j) then ok = false
          j += 1
        if ok then
          found += 1
          pos += n
        else pos += 1
      found
    },
    "indexOfSearch" -> { () =>
      val n = needle.length
      var pos = 0
      var found = 0
      while pos <= haystack.length - n do
        val hit = haystack.indexOf(needle, pos)
        if hit == -1 then pos = haystack.length
        else
          found += 1
          pos = hit + n
      found
    },
  )

@main def main(): Unit = ()
