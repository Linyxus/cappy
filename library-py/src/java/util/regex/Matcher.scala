package java.util.regex

import scala.annotation.switch

import java.lang.StringBuilder
import java.util.function.Function

import scala.python.runtime.PyMatch

final class Matcher private[regex] (
    private var pattern0: Pattern,
    private var input0: String
) extends AnyRef with MatchResult {

  import Matcher._

  def pattern(): Pattern =
    pattern0

  private var regionStart0 = 0
  private var regionEnd0 = input0.length()
  private var inputstr = input0

  private var position = 0
  private var lastMatch: PyMatch | Null = null

  private var appendPos = 0

  def matches(): Boolean = {
    resetMatch()
    lastMatch = pattern().execMatches(inputstr)
    lastMatch != null
  }

  def lookingAt(): Boolean = {
    resetMatch()
    lastMatch = pattern().execFind(inputstr, 0)
    if (lastMatch != null && ensureLastMatch.index != 0)
      resetMatch()
    lastMatch != null
  }

  def find(): Boolean = {
    val mtch = pattern().execFind(inputstr, position)
    if (mtch != null) {
      val end = mtch.end(0)
      position =
        if (end == mtch.index) end + 1
        else end
      lastMatch = mtch
      true
    } else {
      position = inputstr.length() + 1
      lastMatch = null
      false
    }
  }

  def find(start: Int): Boolean = {
    reset()
    position = start
    find()
  }

  def appendReplacement(sb: StringBuffer, replacement: String): Matcher =
    appendReplacementGeneric(sb, replacement)

  def appendReplacement(sb: StringBuilder, replacement: String): Matcher =
    appendReplacementGeneric(sb, replacement)

  private def appendReplacementGeneric(sb: Appendable, replacement: String): Matcher = {
    sb.append(inputstr.substring(appendPos, start()))

    // Java replacement syntax (JDK Matcher.appendReplacement):
    //   $<n>      — numeric group reference (n is a sequence of digits).
    //   ${name}   — named group reference.
    //   \c        — literal `c` (including `\\`, `\$`).
    // A bare `$` followed by anything other than a digit or `{`, and
    // `$` at end-of-string, is an IllegalArgumentException per Java.
    val replacementLen = replacement.length()
    var i = 0
    while (i < replacementLen) {
      val current = replacement.charAt(i)
      if (current == '$') {
        if (i + 1 >= replacementLen)
          throw new IllegalArgumentException(
            "Illegal group reference: group index is missing"
          )
        val next = replacement.charAt(i + 1)
        if (next == '{') {
          val nameStart = i + 2
          val nameEnd = replacement.indexOf('}', nameStart)
          if (nameEnd < 0)
            throw new IllegalArgumentException(
              "Named capturing group is missing trailing '}'"
            )
          val name = replacement.substring(nameStart, nameEnd)
          if (name.isEmpty)
            throw new IllegalArgumentException("Named capturing group has zero length name")
          val replaced = this.group(name)
          if (replaced != null)
            sb.append(replaced)
          i = nameEnd + 1
        } else if (next >= '0' && next <= '9') {
          i = i + 1
          val j = i
          while (i < replacementLen && replacement.charAt(i) >= '0' && replacement.charAt(i) <= '9')
            i = i + 1
          val group = Integer.parseInt(replacement.substring(j, i))
          val replaced = this.group(group)
          if (replaced != null)
            sb.append(replaced)
        } else {
          throw new IllegalArgumentException(
            "Illegal group reference: expected digit or '{' after '$'"
          )
        }
      } else if (current == '\\') {
        if (i + 1 >= replacementLen)
          throw new IllegalArgumentException(
            "character to be escaped is missing"
          )
        sb.append(replacement.charAt(i + 1))
        i = i + 2
      } else {
        sb.append(current)
        i = i + 1
      }
    }

    appendPos = end()
    this
  }

  def appendTail(sb: StringBuffer): StringBuffer = {
    sb.append(inputstr.substring(appendPos))
    appendPos = inputstr.length()
    sb
  }

  def appendTail(sb: StringBuilder): StringBuilder = {
    sb.append(inputstr.substring(appendPos))
    appendPos = inputstr.length()
    sb
  }

  def replaceFirst(replacement: String): String =
    doReplace(replacement, replaceAll = false)

  def replaceFirst(replacer: Function[MatchResult, String]): String = {
    reset()
    if (find()) {
      val sb = new StringBuilder()
      appendReplacement(sb, replacer(this))
      appendTail(sb)
      sb.toString()
    } else {
      inputstr
    }
  }

  def replaceAll(replacement: String): String =
    doReplace(replacement, replaceAll = true)

  def replaceAll(replacer: Function[MatchResult, String]): String = {
    reset()
    val sb = new StringBuilder()
    while (find())
      appendReplacement(sb, replacer(this))
    appendTail(sb)
    sb.toString()
  }

  private def doReplace(replacement: String, replaceAll: Boolean): String = {
    reset()
    val sb = new StringBuilder()
    var replaced = false
    while (find() && (replaceAll || !replaced)) {
      appendReplacement(sb, replacement)
      replaced = true
    }
    appendTail(sb)
    sb.toString()
  }

  private def resetMatch(): Matcher = {
    position = 0
    lastMatch = null
    appendPos = 0
    this
  }

  def reset(): Matcher = {
    regionStart0 = 0
    regionEnd0 = input0.length()
    inputstr = input0
    resetMatch()
  }

  def reset(input: CharSequence): Matcher = {
    input0 = input.toString()
    reset()
  }

  def usePattern(pattern: Pattern): Matcher = {
    pattern0 = pattern
    lastMatch = null
    this
  }

  private def ensureLastMatch: PyMatch = {
    if (lastMatch == null)
      throw new IllegalStateException("No match available")
    lastMatch.asInstanceOf[PyMatch]
  }

  def groupCount(): Int =
    pattern().groupCount

  def start(): Int =
    ensureLastMatch.index + regionStart()

  def end(): Int =
    end(0)

  def group(): String =
    ensureLastMatch.matched()

  private def startInternal(compiledGroup: Int): Int = {
    val start = ensureLastMatch.start(compiledGroup)
    if (start < 0) -1 else start + regionStart()
  }

  def start(group: Int): Int =
    startInternal(pattern().numberedGroup(group))

  def start(name: String): Int =
    val start = ensureLastMatch.startByName(name)
    if (start < 0) -1 else start + regionStart()

  private def endInternal(compiledGroup: Int): Int = {
    val end = ensureLastMatch.end(compiledGroup)
    if (end < 0) -1 else end + regionStart()
  }

  def end(group: Int): Int =
    endInternal(pattern().numberedGroup(group))

  def end(name: String): Int =
    val end = ensureLastMatch.endByName(name)
    if (end < 0) -1 else end + regionStart()

  def group(group: Int): String =
    ensureLastMatch.group(pattern().numberedGroup(group)).asInstanceOf[String]

  def group(name: String): String =
    ensureLastMatch.groupByName(name).asInstanceOf[String]

  def toMatchResult(): MatchResult =
    new SealedResult(lastMatch, pattern(), regionStart())

  def regionStart(): Int =
    regionStart0

  def regionEnd(): Int =
    regionEnd0

  def region(start: Int, end: Int): Matcher = {
    regionStart0 = start
    regionEnd0 = end
    inputstr = input0.substring(start, end)
    resetMatch()
  }

  def hasTransparentBounds(): Boolean =
    false

  def hasAnchoringBounds(): Boolean =
    true
}

object Matcher {
  def quoteReplacement(s: String): String = {
    val result = new StringBuilder()
    var i = 0
    while (i < s.length()) {
      val c = s.charAt(i)
      result.append((c: @switch) match {
        case '\\' | '$' => "\\" + c
        case _          => String.valueOf(c)
      })
      i += 1
    }
    result.toString()
  }

  private final class SealedResult(
      private val lastMatch: PyMatch | Null,
      private val pattern: Pattern,
      private val regionStart: Int
  ) extends MatchResult {

    def groupCount(): Int =
      pattern.groupCount

    private def ensureLastMatch: PyMatch = {
      if (lastMatch == null)
        throw new IllegalStateException("No match available")
      lastMatch.asInstanceOf[PyMatch]
    }

    def start(): Int =
      ensureLastMatch.index + regionStart

    def end(): Int = {
      val end = ensureLastMatch.end(0)
      if (end < 0) -1 else end + regionStart
    }

    def group(): String =
      ensureLastMatch.matched()

    def start(group: Int): Int = {
      val start = ensureLastMatch.start(pattern.numberedGroup(group))
      if (start < 0) -1 else start + regionStart
    }

    def end(group: Int): Int = {
      val end = ensureLastMatch.end(pattern.numberedGroup(group))
      if (end < 0) -1 else end + regionStart
    }

    def group(group: Int): String =
      ensureLastMatch.group(pattern.numberedGroup(group)).asInstanceOf[String]
  }
}
