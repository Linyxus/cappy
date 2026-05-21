package dotty.tools.benchmarks.py.text

/** ROT13 Caesar cipher applied through a `StringBuilder` and through an
 *  in-place `Array[Char]`. Heavy `_scpy_i32` char-arithmetic chains. */
class CaesarCipherBench:
  var size: Int = 0
  var text: String = ""

  def setup(size: Int): Unit =
    this.size = size
    val sb = new StringBuilder(size)
    var i = 0
    while i < size do
      if i % 2 == 0 then sb.append(('A' + (i % 26)).toChar)
      else sb.append(('a' + (i % 26)).toChar)
      i += 1
    text = sb.toString

  private def rot(c: Char): Char =
    val shift = 13
    if c >= 'a' && c <= 'z' then ('a' + (c - 'a' + shift) % 26).toChar
    else if c >= 'A' && c <= 'Z' then ('A' + (c - 'A' + shift) % 26).toChar
    else c

  val operations: Map[String, () => Any] = Map(
    "caesarBuilder" -> { () =>
      val sb = new StringBuilder(text.length)
      var i = 0
      while i < text.length do
        sb.append(rot(text.charAt(i)))
        i += 1
      sb.toString
    },
    "caesarArray" -> { () =>
      val arr = text.toCharArray()
      var i = 0
      while i < arr.length do
        arr(i) = rot(arr(i))
        i += 1
      new String(arr)
    },
  )

@main def main(): Unit = ()
