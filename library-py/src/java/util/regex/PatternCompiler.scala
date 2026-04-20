package java.util.regex

import java.lang.StringBuilder

import scala.python.runtime.PyRegex

import Pattern._

private[regex] object PatternCompiler {
  def compile(regex: String, flags: Int): Pattern =
    new PatternCompiler(regex, flags).compile()

  private def charToFlag(c: Char): Int = c match {
    case 'i' => CASE_INSENSITIVE
    case 'd' => UNIX_LINES
    case 'm' => MULTILINE
    case 's' => DOTALL
    case 'u' => UNICODE_CASE
    case 'x' => COMMENTS
    case 'U' => UNICODE_CHARACTER_CLASS
    case _   => throw new IllegalArgumentException("bad in-pattern flag")
  }

  private def isEmbeddedFlagChar(c: Char): Boolean =
    c == 'i' || c == 'd' || c == 'm' || c == 's' || c == 'u' || c == 'x' || c == 'U'

  private def asciiPosixClass(property: String): String | Null = property match {
    case "Lower"  => "a-z"
    case "Upper"  => "A-Z"
    case "ASCII"  => "\u0000-\u007f"
    case "Alpha"  => "A-Za-z"
    case "Digit"  => "0-9"
    case "Alnum"  => "0-9A-Za-z"
    case "Punct"  => "!-/:-@[-`{-~"
    case "Graph"  => "!-~"
    case "Print"  => " -~"
    case "Blank"  => "\t "
    case "Cntrl"  => "\u0000-\u001f\u007f"
    case "XDigit" => "0-9A-Fa-f"
    case "Space"  => "\t-\r "
    case _        => null
  }

  private def javaPropertyAliasName(property: String): String | Null = property match {
    case "javaAlphabetic" => "Alphabetic"
    case "javaDefined"    => "Cn"
    case "javaDigit"      => "Nd"
    case "javaIdeographic" => "Ideographic"
    case "javaLowerCase"  => "Lowercase"
    case "javaMirrored"   => "Bidi_Mirrored"
    case "javaSpaceChar"  => "Z"
    case "javaTitleCase"  => "Lt"
    case "javaUpperCase"  => "Uppercase"
    case _                => null
  }

  private def javaPropertyAliasPositive(property: String): Boolean =
    property != "javaDefined"

  private def javaPropertyAliasClass(property: String): String | Null = property match {
    case "javaIdentifierIgnorable" =>
      "\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"
    case "javaISOControl" =>
      "\u0000-\u001F\u007F-\u009F"
    case "javaJavaIdentifierPart" =>
      "\\p{L}\\p{Sc}\\p{Pc}\\p{Nd}\\p{Nl}\\p{Mn}\\p{Mc}\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"
    case "javaJavaIdentifierStart" =>
      "\\p{L}\\p{Sc}\\p{Pc}\\p{Nl}"
    case "javaLetterOrDigit" =>
      "\\p{L}\\p{Nd}"
    case "javaUnicodeIdentifierPart" =>
      "\\p{ID_Continue}\u2E2F\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"
    case "javaUnicodeIdentifierStart" =>
      "\\p{ID_Start}\u2E2F"
    case "javaWhitespace" =>
      "\t-\r\u001C-\u001F \u1680\u2000-\u2006\u2008-\u200A\u205F\u3000\\p{Zl}\\p{Zp}"
    case _ =>
      null
  }
}

private final class PatternCompiler(private val pattern: String, private var flags: Int) {
  import PatternCompiler._

  private var pIndex = 0
  private var sticky = false
  private var groupCount = 0
  private val namedGroups = new java.util.HashMap[String, Int]()

  private def hasFlag(flag: Int): Boolean =
    (flags & flag) != 0

  private def unicodeCharacterClass: Boolean =
    hasFlag(UNICODE_CHARACTER_CLASS)

  private def unicodeCaseInsensitive: Boolean =
    hasFlag(CASE_INSENSITIVE) && hasFlag(UNICODE_CASE)

  private def needsExplicitAsciiClasses: Boolean =
    unicodeCaseInsensitive && !unicodeCharacterClass

  def compile(): Pattern = {
    if (hasFlag(UNICODE_CHARACTER_CLASS))
      flags |= UNICODE_CASE

    val isLiteral = hasFlag(LITERAL)
    if (!isLiteral)
      processLeadingEmbeddedFlags()

    if (hasFlag(CANON_EQ))
      parseError("CANON_EQ is not supported")

    val translated =
      if (isLiteral) {
        PyRegex.escape(pattern)
      } else {
        if (pattern.startsWith("\\G", pIndex)) {
          sticky = true
          pIndex += 2
        }
        translatePattern()
      }

    val pyFlags = computePythonFlags()
    try {
      val compiledPattern = PyRegex.compileWithFlags(translated, pyFlags)
      new Pattern(
        pattern,
        flags,
        translated,
        pyFlags,
        sticky,
        groupCount,
        compiledPattern,
        namedGroups
      )
    } catch {
      case pse: PatternSyntaxException =>
        throw pse
      case t =>
        throw translateCompileFailure(t)
    }
  }

  private def computePythonFlags(): Int = {
    var out = PyRegex.Version1Flag

    if (hasFlag(CASE_INSENSITIVE))
      out |= PyRegex.IgnoreCaseFlag
    if (hasFlag(MULTILINE))
      out |= PyRegex.MultilineFlag
    if (hasFlag(DOTALL))
      out |= PyRegex.DotAllFlag
    if (hasFlag(COMMENTS))
      out |= PyRegex.VerboseFlag

    out
  }

  private def translatePattern(): String = {
    val out = new StringBuilder()
    var classDepth = 0

    while (pIndex < pattern.length()) {
      val c = pattern.charAt(pIndex)
      if (c == '\\') {
        translateEscape(out, classDepth > 0)
      } else {
        if (c == '[')
          classDepth += 1
        else if (c == ']' && classDepth > 0)
          classDepth -= 1
        else if (c == '(' && classDepth == 0)
          noteGroupStart()

        out.append(c)
        pIndex += 1
      }
    }

    out.toString()
  }

  private def translateEscape(out: StringBuilder, inCharClass: Boolean): Unit = {
    if (pIndex + 1 >= pattern.length())
      parseError("\\ at end of pattern")

    val dispatch = pattern.charAt(pIndex + 1)
    if (dispatch == 'Q') {
      appendQuotedLiteral(out, inCharClass)
    } else if (dispatch == 'k' && !inCharClass) {
      appendNamedBackReference(out)
    } else if (dispatch == 'p' || dispatch == 'P') {
      appendPropertyEscape(out, dispatch == 'p', inCharClass)
    } else if (
      !unicodeCharacterClass &&
      (dispatch == 'd' || dispatch == 'D' || dispatch == 's' || dispatch == 'S' ||
        dispatch == 'w' || dispatch == 'W' || dispatch == 'b' || dispatch == 'B')
    ) {
      out.append(explicitAsciiEscape(dispatch))
      pIndex = pIndex + 2
    } else {
      out.append('\\')
      out.append(dispatch)
      pIndex = pIndex + 2
    }
  }

  private def appendQuotedLiteral(out: StringBuilder, inCharClass: Boolean): Unit = {
    val start = pIndex + 2
    val end = pattern.indexOf("\\E", start)
    val literalEnd = if (end < 0) pattern.length() else end
    var i = start
    while (i < literalEnd) {
      out.append(escapeLiteral(pattern.charAt(i), inCharClass))
      i += 1
    }
    pIndex =
      if (end < 0) pattern.length()
      else end + 2
  }

  private def appendNamedBackReference(out: StringBuilder): Unit = {
    if (!pattern.startsWith("\\k<", pIndex))
      parseError("Illegal named back reference")
    val nameStart = pIndex + 3
    val nameEnd = pattern.indexOf('>', nameStart)
    if (nameEnd < 0)
      parseError("Unclosed named capturing group reference")
    val name = pattern.substring(nameStart, nameEnd)
    out.append("(?P=")
    out.append(name)
    out.append(")")
    pIndex = nameEnd + 1
  }

  private def appendPropertyEscape(out: StringBuilder, positive: Boolean, inCharClass: Boolean): Unit = {
    val start = pIndex + 2
    var property = "?"
    if (start >= pattern.length()) {
      pIndex = start
    } else if (pattern.charAt(start) == '{') {
      val close = pattern.indexOf('}', start + 1)
      if (close < 0)
        parseError("Unclosed character family")
      pIndex = close + 1
      property = pattern.substring(start + 1, close)
    } else {
      pIndex = start + 1
      property = pattern.substring(start, start + 1)
    }

    out.append(renderProperty(property, positive, inCharClass))
  }

  private def renderProperty(property: String, positive: Boolean, inCharClass: Boolean): String = {
    if (property.startsWith("In") || property.startsWith("blk=") || property.startsWith("block="))
      parseError("Blocks are not supported in \\p Unicode character families")

    val asciiPosix = asciiPosixClass(property)
    if (!unicodeCharacterClass && asciiPosix != null) {
      renderCharClassContent(asciiPosix, positive, inCharClass)
    } else {
      val aliasClass = javaPropertyAliasClass(property)
      if aliasClass != null then
        renderCharClassContent(aliasClass, positive, inCharClass)
      else
        val aliasName = javaPropertyAliasName(property)
        if aliasName != null then
          renderNativeProperty(aliasName, positive == javaPropertyAliasPositive(property))
        else
          renderNativeProperty(property, positive)
    }
  }

  private def renderNativeProperty(property: String, positive: Boolean): String =
    if (positive) s"\\p{$property}" else s"\\P{$property}"

  private def renderCharClassContent(content: String, positive: Boolean, inCharClass: Boolean): String = {
    if (positive) {
      if (inCharClass) content else s"[$content]"
    } else {
      s"[^$content]"
    }
  }

  private def explicitAsciiEscape(dispatch: Char): String = dispatch match {
    case 'd' => "[0-9]"
    case 'D' => "[^0-9]"
    case 's' => "[\t-\r ]"
    case 'S' => "[^\t-\r ]"
    case 'w' => "[A-Za-z_0-9]"
    case 'W' => "[^A-Za-z_0-9]"
    case 'b' => "(?:(?<=[A-Za-z_0-9])(?=[^A-Za-z_0-9])|(?<=[^A-Za-z_0-9])(?=[A-Za-z_0-9])|\\A(?=[A-Za-z_0-9])|(?<=[A-Za-z_0-9])\\z)"
    case 'B' => "(?:(?<=[A-Za-z_0-9])(?=[A-Za-z_0-9])|(?<=[^A-Za-z_0-9])(?=[^A-Za-z_0-9])|\\A(?=[^A-Za-z_0-9])|(?<=[^A-Za-z_0-9])\\z)"
  }

  private def escapeLiteral(c: Char, inCharClass: Boolean): String = {
    if (inCharClass) {
      if (c == '\\' || c == ']' || c == '-' || c == '^' || c == '[') "\\" + c
      else String.valueOf(c)
    } else {
      if (
        c == '^' || c == '$' || c == '\\' || c == '.' || c == '*' || c == '+' || c == '?' ||
          c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' || c == '|'
      ) "\\" + c
      else String.valueOf(c)
    }
  }

  private def noteGroupStart(): Unit = {
    if (pIndex + 1 >= pattern.length() || pattern.charAt(pIndex + 1) != '?') {
      groupCount += 1
      return
    }

    if (pIndex + 2 >= pattern.length())
      return

    pattern.charAt(pIndex + 2) match {
      case ':' | '=' | '!' | '>' | '#' | 'i' | 'd' | 'm' | 's' | 'u' | 'x' | 'U' | '-' =>
        ()
      case '<' =>
        if (pIndex + 3 < pattern.length() && (pattern.charAt(pIndex + 3) == '=' || pattern.charAt(pIndex + 3) == '!')) {
          ()
        } else {
          val nameEnd = pattern.indexOf('>', pIndex + 3)
          if (nameEnd < 0)
            parseError("Unclosed named capturing group")
          val name = pattern.substring(pIndex + 3, nameEnd)
          groupCount += 1
          if (namedGroups.containsKey(name))
            parseError(s"Named capturing group <$name> is already defined")
          namedGroups.put(name, groupCount)
        }
      case _ =>
        groupCount += 1
    }
  }

  private def processLeadingEmbeddedFlags(): Unit = {
    if (!pattern.startsWith("(?", pIndex))
      return

    var i = pIndex + 2
    val addStart = i
    while (i < pattern.length() && isEmbeddedFlagChar(pattern.charAt(i)))
      i += 1

    val added = pattern.substring(addStart, i)
    var removed = ""

    if (i < pattern.length() && pattern.charAt(i) == '-') {
      i += 1
      val removeStart = i
      while (i < pattern.length() && isEmbeddedFlagChar(pattern.charAt(i)))
        i += 1
      removed = pattern.substring(removeStart, i)
    }

    if ((added.isEmpty && removed.isEmpty) || i >= pattern.length() || pattern.charAt(i) != ')')
      return

    var j = 0
    while (j < added.length()) {
      flags |= charToFlag(added.charAt(j))
      j += 1
    }

    if (hasFlag(UNICODE_CHARACTER_CLASS))
      flags |= UNICODE_CASE

    j = 0
    while (j < removed.length()) {
      flags &= ~charToFlag(removed.charAt(j))
      j += 1
    }

    pIndex = i + 1
  }

  private def translateCompileFailure(t: Any): PatternSyntaxException = {
    val message =
      t match {
        case throwable: Throwable => String.valueOf(throwable.getMessage())
        case other                => String.valueOf(other)
      }
    val index = extractIndex(message)
    val desc =
      if (message == null || message == "null") "Invalid regular expression"
      else message
    new PatternSyntaxException(desc, pattern, index)
  }

  private def extractIndex(message: String): Int = {
    val marker = "position "
    val at = message.indexOf(marker)
    if (at < 0) {
      -1
    } else {
      val start = at + marker.length()
      var end = start
      while (end < message.length() && Character.isDigit(message.charAt(end)))
        end += 1
      if (end == start) -1
      else Integer.parseInt(message.substring(start, end))
    }
  }

  private def parseError(desc: String): Nothing =
    throw new PatternSyntaxException(desc, pattern, pIndex)
}
