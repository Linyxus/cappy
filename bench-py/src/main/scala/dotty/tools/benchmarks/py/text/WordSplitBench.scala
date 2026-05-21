package dotty.tools.benchmarks.py.text

import scala.collection.mutable.ArrayBuffer

/** Word-boundary scanning: manual split into an `ArrayBuffer`, an
 *  allocation-free word counter, and word-order reversal via `StringBuilder`. */
class WordSplitBench:
  var size: Int = 0
  var sentence: String = ""
  var words: Array[String] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val sb = new StringBuilder()
    var i = 0
    while i < size do
      if i != 0 then sb.append(' ')
      sb.append("word")
      sb.append(i % 100)
      i += 1
    sentence = sb.toString
    words = sentence.split(" ")

  val operations: Map[String, () => Any] = Map(
    "manualSplit" -> { () =>
      val buf = ArrayBuffer.empty[String]
      var start = 0
      var i = 0
      while i <= sentence.length do
        if i == sentence.length || sentence.charAt(i) == ' ' then
          if i > start then buf += sentence.substring(start, i)
          start = i + 1
        i += 1
      buf.length
    },
    "charCountWords" -> { () =>
      var inWord = false
      var count = 0
      var i = 0
      while i < sentence.length do
        val isSpace = sentence.charAt(i) == ' '
        if !isSpace && !inWord then count += 1
        inWord = !isSpace
        i += 1
      count
    },
    "reverseWords" -> { () =>
      val sb = new StringBuilder()
      var i = words.length - 1
      while i >= 0 do
        if i < words.length - 1 then sb.append(' ')
        sb.append(words(i))
        i -= 1
      sb.toString
    },
  )

@main def main(): Unit = ()
