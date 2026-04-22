package java.util.regex

import java.lang.StringBuilder

import scala.python.runtime.{PyMatch, PyPattern}

final class Pattern private[regex] (
    private val rawPattern: String,
    private val rawFlags: Int,
    private[regex] val translatedPattern: String,
    private[regex] val pythonFlags: Int,
    private[regex] val sticky: Boolean,
    private[regex] val groupCount: Int,
    private val compiled: PyPattern,
    private val namedGroups: java.util.HashMap[String, Int]
) extends Serializable {

  private[regex] def execMatches(input: String): PyMatch | Null =
    compiled.fullMatch(input)

  private[regex] def execFind(input: String, start: Int): PyMatch | Null =
    if (sticky) compiled.firstMatch(input, start)
    else compiled.search(input, start)

  private[regex] def numberedGroup(group: Int): Int = {
    java.lang.BoundsChecks.checkIndexInclusive(group, groupCount)
    group
  }

  private[regex] def namedGroup(name: String): Int =
    if namedGroups.containsKey(name) then
      namedGroups.get(name)
    else
      throw new IllegalArgumentException(s"No group with name <$name>")

  def pattern(): String =
    rawPattern

  def flags(): Int =
    rawFlags

  override def toString(): String =
    pattern()

  def matcher(input: CharSequence): Matcher =
    new Matcher(this, input.toString())

  def split(input: CharSequence): Array[String] =
    split(input, 0)

  def split(input: CharSequence, limit: Int): Array[String] =
    split(input.toString(), limit)

  private def split(inputStr: String, limit: Int): Array[String] = {
    if (inputStr == "") {
      Array("")
    } else {
      val lim = if (limit > 0) limit else Int.MaxValue
      val matcher = this.matcher(inputStr)
      val result = new java.util.ArrayList[String]()
      var prevEnd = 0
      while (result.size() < lim - 1 && matcher.find()) {
        if (matcher.end() != 0)
          result.add(inputStr.substring(prevEnd, matcher.start()))
        prevEnd = matcher.end()
      }
      result.add(inputStr.substring(prevEnd))

      var actualLength = result.size()
      if (limit == 0) {
        while (actualLength != 0 && result.get(actualLength - 1).length() == 0)
          actualLength -= 1
      }

      val out = new Array[String](actualLength)
      var i = 0
      while (i < actualLength) {
        out(i) = result.get(i)
        i += 1
      }
      out
    }
  }
}

object Pattern {
  final val UNIX_LINES = 0x01
  final val CASE_INSENSITIVE = 0x02
  final val COMMENTS = 0x04
  final val MULTILINE = 0x08
  final val LITERAL = 0x10
  final val DOTALL = 0x20
  final val UNICODE_CASE = 0x40
  final val CANON_EQ = 0x80
  final val UNICODE_CHARACTER_CLASS = 0x100

  def compile(regex: String, flags: Int): Pattern =
    PatternCompiler.compile(regex, flags)

  def compile(regex: String): Pattern =
    compile(regex, 0)

  def matches(regex: String, input: CharSequence): Boolean =
    matches(regex, input.toString())

  private def matches(regex: String, input: String): Boolean =
    compile(regex).matcher(input).matches()

  def quote(s: String): String = {
    val result = new StringBuilder("\\Q")
    var start = 0
    var end = s.indexOf("\\E", start)
    while (end >= 0) {
      result.append(s.substring(start, end))
      result.append("\\E\\\\E\\Q")
      start = end + 2
      end = s.indexOf("\\E", start)
    }
    result.append(s.substring(start))
    result.append("\\E")
    result.toString()
  }
}
