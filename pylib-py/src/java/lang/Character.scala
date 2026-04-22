/*
 * Python-native Character port for the ScalaPy backend.
 *
 * This intentionally does not mirror the Scala.js-generated Unicode tables
 * byte-for-byte. Instead it maps to Python's Unicode database (`unicodedata`)
 * plus the existing builtins facade for code-point operations.
 */

package java.lang

import java.lang.constant.Constable

import scala.python.runtime.{PyBuiltins, PyUnicodeData}

class Character private ()
    extends AnyRef with java.io.Serializable with Comparable[Character] with Constable {

  def this(value: scala.Char) = this()

  @inline def charValue(): scala.Char =
    this.asInstanceOf[scala.Char]

  @inline override def hashCode(): Int =
    Character.hashCode(charValue())

  @inline override def equals(that: Any): scala.Boolean =
    that.isInstanceOf[Character] &&
      (charValue() == that.asInstanceOf[Character].charValue())

  @inline override def toString(): String =
    Character.toString(charValue())

  @inline override def compareTo(that: Character): Int =
    Character.compare(charValue(), that.charValue())
}

object Character {
  def TYPE: Class[?] = scala.Predef.classOf[scala.Char]

  final val MIN_VALUE = '\u0000'
  final val MAX_VALUE = '\uffff'
  final val SIZE = 16
  final val BYTES = 2

  @inline def `new`(value: scala.Char): Character = valueOf(value)
  @inline def valueOf(c: scala.Char): Character = c.asInstanceOf[Character]

  final val UNASSIGNED: scala.Byte = 0
  final val UPPERCASE_LETTER: scala.Byte = 1
  final val LOWERCASE_LETTER: scala.Byte = 2
  final val TITLECASE_LETTER: scala.Byte = 3
  final val MODIFIER_LETTER: scala.Byte = 4
  final val OTHER_LETTER: scala.Byte = 5
  final val NON_SPACING_MARK: scala.Byte = 6
  final val ENCLOSING_MARK: scala.Byte = 7
  final val COMBINING_SPACING_MARK: scala.Byte = 8
  final val DECIMAL_DIGIT_NUMBER: scala.Byte = 9
  final val LETTER_NUMBER: scala.Byte = 10
  final val OTHER_NUMBER: scala.Byte = 11
  final val SPACE_SEPARATOR: scala.Byte = 12
  final val LINE_SEPARATOR: scala.Byte = 13
  final val PARAGRAPH_SEPARATOR: scala.Byte = 14
  final val CONTROL: scala.Byte = 15
  final val FORMAT: scala.Byte = 16
  final val PRIVATE_USE: scala.Byte = 18
  final val SURROGATE: scala.Byte = 19
  final val DASH_PUNCTUATION: scala.Byte = 20
  final val START_PUNCTUATION: scala.Byte = 21
  final val END_PUNCTUATION: scala.Byte = 22
  final val CONNECTOR_PUNCTUATION: scala.Byte = 23
  final val OTHER_PUNCTUATION: scala.Byte = 24
  final val MATH_SYMBOL: scala.Byte = 25
  final val CURRENCY_SYMBOL: scala.Byte = 26
  final val MODIFIER_SYMBOL: scala.Byte = 27
  final val OTHER_SYMBOL: scala.Byte = 28
  final val INITIAL_QUOTE_PUNCTUATION: scala.Byte = 29
  final val FINAL_QUOTE_PUNCTUATION: scala.Byte = 30

  final val MIN_RADIX = 2
  final val MAX_RADIX = 36

  final val MIN_HIGH_SURROGATE = '\uD800'
  final val MAX_HIGH_SURROGATE = '\uDBFF'
  final val MIN_LOW_SURROGATE = '\uDC00'
  final val MAX_LOW_SURROGATE = '\uDFFF'
  final val MIN_SURROGATE = MIN_HIGH_SURROGATE
  final val MAX_SURROGATE = MAX_LOW_SURROGATE

  final val MIN_CODE_POINT = 0
  final val MAX_CODE_POINT = 0x10ffff
  final val MIN_SUPPLEMENTARY_CODE_POINT = 0x10000

  final val DIRECTIONALITY_UNDEFINED: scala.Byte = -1
  final val DIRECTIONALITY_LEFT_TO_RIGHT: scala.Byte = 0
  final val DIRECTIONALITY_RIGHT_TO_LEFT: scala.Byte = 1
  final val DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC: scala.Byte = 2
  final val DIRECTIONALITY_EUROPEAN_NUMBER: scala.Byte = 3
  final val DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR: scala.Byte = 4
  final val DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR: scala.Byte = 5
  final val DIRECTIONALITY_ARABIC_NUMBER: scala.Byte = 6
  final val DIRECTIONALITY_COMMON_NUMBER_SEPARATOR: scala.Byte = 7
  final val DIRECTIONALITY_NONSPACING_MARK: scala.Byte = 8
  final val DIRECTIONALITY_BOUNDARY_NEUTRAL: scala.Byte = 9
  final val DIRECTIONALITY_PARAGRAPH_SEPARATOR: scala.Byte = 10
  final val DIRECTIONALITY_SEGMENT_SEPARATOR: scala.Byte = 11
  final val DIRECTIONALITY_WHITESPACE: scala.Byte = 12
  final val DIRECTIONALITY_OTHER_NEUTRALS: scala.Byte = 13
  final val DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING: scala.Byte = 14
  final val DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE: scala.Byte = 15
  final val DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING: scala.Byte = 16
  final val DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE: scala.Byte = 17
  final val DIRECTIONALITY_POP_DIRECTIONAL_FORMAT: scala.Byte = 18
  final val DIRECTIONALITY_LEFT_TO_RIGHT_ISOLATE: scala.Byte = 19
  final val DIRECTIONALITY_RIGHT_TO_LEFT_ISOLATE: scala.Byte = 20
  final val DIRECTIONALITY_FIRST_STRONG_ISOLATE: scala.Byte = 21
  final val DIRECTIONALITY_POP_DIRECTIONAL_ISOLATE: scala.Byte = 22

  private final val HighSurrogateMask = 0xfc00
  private final val HighSurrogateID = 0xd800
  private final val LowSurrogateMask = 0xfc00
  private final val LowSurrogateID = 0xdc00
  private final val SurrogateMask = 0xf800
  private final val SurrogateID = 0xd800
  private final val SurrogateUsefulPartMask = 0x03ff
  private final val SurrogatePairMask = (HighSurrogateMask << 16) | LowSurrogateMask
  private final val SurrogatePairID = (HighSurrogateID << 16) | LowSurrogateID
  private final val HighSurrogateShift = 10
  private final val HighSurrogateAddValue = 0x10000 >> HighSurrogateShift

  @inline def hashCode(value: Char): Int = value.toInt

  @inline def compare(x: Char, y: Char): Int =
    x.toInt - y.toInt

  @inline def toString(c: Char): String =
    "" + c

  def toString(codePoint: Int): String = {
    if (!isValidCodePoint(codePoint))
      throw new IllegalArgumentException()
    PyBuiltins.chr_of(codePoint)
  }

  @inline def isValidCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= 0) && (codePoint <= MAX_CODE_POINT)

  @inline def isBmpCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= 0) && (codePoint < MIN_SUPPLEMENTARY_CODE_POINT)

  @inline def isSupplementaryCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= MIN_SUPPLEMENTARY_CODE_POINT) && (codePoint <= MAX_CODE_POINT)

  @inline def isHighSurrogate(ch: Char): scala.Boolean =
    (ch & HighSurrogateMask) == HighSurrogateID

  @inline def isLowSurrogate(ch: Char): scala.Boolean =
    (ch & LowSurrogateMask) == LowSurrogateID

  @inline def isSurrogate(ch: Char): scala.Boolean =
    (ch & SurrogateMask) == SurrogateID

  @inline def isSurrogatePair(high: Char, low: Char): scala.Boolean =
    (((high << 16) | low) & SurrogatePairMask) == SurrogatePairID

  @inline def charCount(codePoint: Int): Int =
    if (codePoint >= MIN_SUPPLEMENTARY_CODE_POINT) 2 else 1

  @inline def toCodePoint(high: Char, low: Char): Int =
    (((high & SurrogateUsefulPartMask) + HighSurrogateAddValue) << HighSurrogateShift) |
      (low & SurrogateUsefulPartMask)

  @inline def highSurrogate(codePoint: Int): Char =
    (HighSurrogateID | ((codePoint >> HighSurrogateShift) - HighSurrogateAddValue)).toChar

  @inline def lowSurrogate(codePoint: Int): Char =
    (LowSurrogateID | (codePoint & SurrogateUsefulPartMask)).toChar

  @inline private[java] def isRadixInvalid(radix: Int): scala.Boolean =
    radix < MIN_RADIX || radix > MAX_RADIX

  def codePointAt(seq: CharSequence, index: Int): Int = {
    val high = seq.charAt(index)
    if (isHighSurrogate(high) && (index + 1 < seq.length())) {
      val low = seq.charAt(index + 1)
      if (isLowSurrogate(low)) toCodePoint(high, low)
      else high.toInt
    } else high.toInt
  }

  def codePointAt(a: Array[Char], index: Int): Int =
    codePointAt(CharSequence.ofArray(a), index)

  def codePointBefore(seq: CharSequence, index: Int): Int = {
    val low = seq.charAt(index - 1)
    if (isLowSurrogate(low) && index >= 2) {
      val high = seq.charAt(index - 2)
      if (isHighSurrogate(high)) toCodePoint(high, low)
      else low.toInt
    } else low.toInt
  }

  def codePointBefore(a: Array[Char], index: Int): Int =
    codePointBefore(CharSequence.ofArray(a), index)

  def codePointCount(seq: CharSequence, beginIndex: Int, endIndex: Int): Int = {
    var count = 0
    var i = beginIndex
    while (i < endIndex) {
      val ch = seq.charAt(i)
      if (isHighSurrogate(ch) && i + 1 < endIndex && isLowSurrogate(seq.charAt(i + 1)))
        i += 2
      else
        i += 1
      count += 1
    }
    count
  }

  def codePointCount(a: Array[Char], offset: Int, count: Int): Int =
    codePointCount(CharSequence.ofArray(a), offset, offset + count)

  def offsetByCodePoints(seq: CharSequence, index: Int, codePointOffset: Int): Int = {
    var i = index
    var remaining = codePointOffset
    if (remaining >= 0) {
      while (remaining > 0) {
        val ch = seq.charAt(i)
        if (isHighSurrogate(ch) && i + 1 < seq.length() && isLowSurrogate(seq.charAt(i + 1)))
          i += 2
        else
          i += 1
        remaining -= 1
      }
    } else {
      while (remaining < 0) {
        i -= 1
        val ch = seq.charAt(i)
        if (isLowSurrogate(ch) && i - 1 >= 0 && isHighSurrogate(seq.charAt(i - 1)))
          i -= 1
        remaining += 1
      }
    }
    i
  }

  def offsetByCodePoints(a: Array[Char], start: Int, count: Int, index: Int, codePointOffset: Int): Int =
    offsetByCodePoints(CharSequence.ofArray(a), index, codePointOffset)

  def digit(c: scala.Char, radix: Int): Int =
    digit(c.toInt, radix)

  def digit(codePoint: Int, radix: Int): Int = {
    if (isRadixInvalid(radix)) -1
    else digitWithValidRadix(codePoint, radix)
  }

  private[lang] def digitWithValidRadix(codePoint: Int, radix: Int): Int = {
    val value =
      if (codePoint >= '0' && codePoint <= '9')
        codePoint - '0'
      else if (codePoint >= 'A' && codePoint <= 'Z')
        codePoint - ('A' - 10)
      else if (codePoint >= 'a' && codePoint <= 'z')
        codePoint - ('a' - 10)
      else if (codePoint >= 0xff21 && codePoint <= 0xff3a)
        codePoint - (0xff21 - 10)
      else if (codePoint >= 0xff41 && codePoint <= 0xff5a)
        codePoint - (0xff41 - 10)
      else if (isValidCodePoint(codePoint) && PyBuiltins.is_decimal(toString(codePoint)))
        PyUnicodeData.decimalOrMinusOne(toString(codePoint))
      else -1

    if (value >= 0 && value < radix) value else -1
  }

  private[lang] def isZeroDigit(ch: Char): scala.Boolean =
    digit(ch, 10) == 0

  def forDigit(digit: Int, radix: Int): Char = {
    if (isRadixInvalid(radix) || Integer.unsigned_>=(digit, radix)) 0
    else {
      val overBaseTen = digit - 10
      (if (overBaseTen < 0) '0' + digit else 'a' + overBaseTen).toChar
    }
  }

  def isISOControl(c: scala.Char): scala.Boolean = isISOControl(c.toInt)

  def isISOControl(codePoint: Int): scala.Boolean =
    (0x00 <= codePoint && codePoint <= 0x1f) || (0x7f <= codePoint && codePoint <= 0x9f)

  @deprecated("Replaced by isWhitespace(char)", "")
  def isSpace(c: scala.Char): scala.Boolean =
    c == '\t' || c == '\n' || c == '\f' || c == '\r' || c == ' '

  def isWhitespace(c: scala.Char): scala.Boolean =
    isWhitespace(c.toInt)

  def isWhitespace(codePoint: scala.Int): scala.Boolean =
    isValidCodePoint(codePoint) &&
      codePoint != '\u00A0' &&
      codePoint != '\u2007' &&
      codePoint != '\u202F' &&
      PyBuiltins.is_space(toString(codePoint))

  def isSpaceChar(ch: scala.Char): scala.Boolean =
    isSpaceChar(ch.toInt)

  def isSpaceChar(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    tpe == SPACE_SEPARATOR || tpe == LINE_SEPARATOR || tpe == PARAGRAPH_SEPARATOR
  }

  def isLowerCase(c: scala.Char): scala.Boolean =
    isLowerCase(c.toInt)

  def isLowerCase(c: Int): scala.Boolean =
    isValidCodePoint(c) && PyBuiltins.is_lower(toString(c))

  def isUpperCase(c: scala.Char): scala.Boolean =
    isUpperCase(c.toInt)

  def isUpperCase(c: Int): scala.Boolean =
    isValidCodePoint(c) && PyBuiltins.is_upper(toString(c))

  def isTitleCase(c: scala.Char): scala.Boolean =
    isTitleCase(c.toInt)

  def isTitleCase(cp: Int): scala.Boolean =
    getType(cp) == TITLECASE_LETTER

  def isDigit(c: scala.Char): scala.Boolean =
    isDigit(c.toInt)

  def isDigit(cp: Int): scala.Boolean =
    isValidCodePoint(cp) && PyBuiltins.is_decimal(toString(cp))

  def isDefined(c: scala.Char): scala.Boolean =
    isDefined(c.toInt)

  def isDefined(c: scala.Int): scala.Boolean =
    isValidCodePoint(c) && getType(c) != UNASSIGNED

  def isLetter(c: scala.Char): scala.Boolean =
    isLetter(c.toInt)

  def isLetter(cp: Int): scala.Boolean = {
    val tpe = getType(cp)
    tpe == UPPERCASE_LETTER || tpe == LOWERCASE_LETTER ||
      tpe == TITLECASE_LETTER || tpe == MODIFIER_LETTER || tpe == OTHER_LETTER
  }

  def isLetterOrDigit(c: scala.Char): scala.Boolean =
    isLetterOrDigit(c.toInt)

  def isLetterOrDigit(cp: Int): scala.Boolean =
    isLetter(cp) || isDigit(cp)

  def isAlphabetic(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    tpe == UPPERCASE_LETTER || tpe == LOWERCASE_LETTER ||
      tpe == TITLECASE_LETTER || tpe == MODIFIER_LETTER ||
      tpe == OTHER_LETTER || tpe == LETTER_NUMBER
  }

  def isIdeographic(c: Int): scala.Boolean = {
    (12294 <= c && c <= 12295) || (12321 <= c && c <= 12329) ||
    (12344 <= c && c <= 12346) || (13312 <= c && c <= 19893) ||
    (19968 <= c && c <= 40908) || (63744 <= c && c <= 64109) ||
    (64112 <= c && c <= 64217) || (131072 <= c && c <= 173782) ||
    (173824 <= c && c <= 177972) || (177984 <= c && c <= 178205) ||
    (194560 <= c && c <= 195101)
  }

  def isJavaIdentifierStart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierStart(ch.toInt)

  def isJavaIdentifierStart(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    isLetter(codePoint) || tpe == LETTER_NUMBER || tpe == CURRENCY_SYMBOL ||
      tpe == CONNECTOR_PUNCTUATION
  }

  def isJavaIdentifierPart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierPart(ch.toInt)

  def isJavaIdentifierPart(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    isLetter(codePoint) || tpe == CURRENCY_SYMBOL ||
      tpe == CONNECTOR_PUNCTUATION || tpe == DECIMAL_DIGIT_NUMBER ||
      tpe == LETTER_NUMBER || tpe == COMBINING_SPACING_MARK ||
      tpe == NON_SPACING_MARK || isIdentifierIgnorable(codePoint)
  }

  def isUnicodeIdentifierStart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierStart(ch.toInt)

  def isUnicodeIdentifierStart(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    isLetter(codePoint) || tpe == LETTER_NUMBER
  }

  def isUnicodeIdentifierPart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierPart(ch.toInt)

  def isUnicodeIdentifierPart(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    tpe == CONNECTOR_PUNCTUATION || tpe == DECIMAL_DIGIT_NUMBER ||
      tpe == COMBINING_SPACING_MARK || tpe == NON_SPACING_MARK ||
      isUnicodeIdentifierStart(codePoint) || isIdentifierIgnorable(codePoint)
  }

  def isIdentifierIgnorable(c: scala.Char): scala.Boolean =
    isIdentifierIgnorable(c.toInt)

  def isIdentifierIgnorable(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    ('\u0000' <= codePoint && codePoint <= '\u0008') ||
    ('\u000E' <= codePoint && codePoint <= '\u001B') ||
    ('\u007F' <= codePoint && codePoint <= '\u009F') ||
    tpe == FORMAT
  }

  def isMirrored(c: scala.Char): scala.Boolean =
    isMirrored(c.toInt)

  def isMirrored(codePoint: Int): scala.Boolean =
    isValidCodePoint(codePoint) && PyUnicodeData.mirrored(toString(codePoint)) != 0

  def toUpperCase(ch: Char): Char = toUpperCase(ch.toInt).toChar

  def toUpperCase(codePoint: scala.Int): scala.Int =
    mapCase(codePoint, _.toUpperCase())

  def toLowerCase(ch: scala.Char): scala.Char = toLowerCase(ch.toInt).toChar

  def toLowerCase(codePoint: scala.Int): scala.Int =
    mapCase(codePoint, _.toLowerCase())

  def toTitleCase(ch: scala.Char): scala.Char = toTitleCase(ch.toInt).toChar

  def toTitleCase(codePoint: scala.Int): scala.Int =
    mapCase(codePoint, PyBuiltins.capitalize)

  def getType(ch: scala.Char): scala.Int =
    getType(ch.toInt)

  def getType(codePoint: Int): scala.Int = {
    if (!isValidCodePoint(codePoint)) UNASSIGNED.toInt
    else mapCategory(PyUnicodeData.category(toString(codePoint))).toInt
  }

  def getNumericValue(ch: scala.Char): scala.Int =
    getNumericValue(ch.toInt)

  def getNumericValue(codePoint: Int): scala.Int = {
    val digitValue = digit(codePoint, 10)
    if (digitValue >= 0) digitValue
    else if (!isValidCodePoint(codePoint)) -1
    else {
      val numeric = PyUnicodeData.numericOrNaN(toString(codePoint))
      if (numeric != numeric) -1
      else {
        val asInt = numeric.toInt
        if (asInt.toDouble == numeric) asInt else -2
      }
    }
  }

  def getDirectionality(ch: scala.Char): scala.Byte =
    getDirectionality(ch.toInt)

  def getDirectionality(codePoint: Int): scala.Byte = {
    if (!isValidCodePoint(codePoint)) DIRECTIONALITY_UNDEFINED
    else
      PyUnicodeData.bidirectional(toString(codePoint)) match {
        case "L"   => DIRECTIONALITY_LEFT_TO_RIGHT
        case "R"   => DIRECTIONALITY_RIGHT_TO_LEFT
        case "AL"  => DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC
        case "EN"  => DIRECTIONALITY_EUROPEAN_NUMBER
        case "ES"  => DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR
        case "ET"  => DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
        case "AN"  => DIRECTIONALITY_ARABIC_NUMBER
        case "CS"  => DIRECTIONALITY_COMMON_NUMBER_SEPARATOR
        case "NSM" => DIRECTIONALITY_NONSPACING_MARK
        case "BN"  => DIRECTIONALITY_BOUNDARY_NEUTRAL
        case "B"   => DIRECTIONALITY_PARAGRAPH_SEPARATOR
        case "S"   => DIRECTIONALITY_SEGMENT_SEPARATOR
        case "WS"  => DIRECTIONALITY_WHITESPACE
        case "ON"  => DIRECTIONALITY_OTHER_NEUTRALS
        case "LRE" => DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING
        case "LRO" => DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE
        case "RLE" => DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING
        case "RLO" => DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE
        case "PDF" => DIRECTIONALITY_POP_DIRECTIONAL_FORMAT
        case "LRI" => DIRECTIONALITY_LEFT_TO_RIGHT_ISOLATE
        case "RLI" => DIRECTIONALITY_RIGHT_TO_LEFT_ISOLATE
        case "FSI" => DIRECTIONALITY_FIRST_STRONG_ISOLATE
        case "PDI" => DIRECTIONALITY_POP_DIRECTIONAL_ISOLATE
        case _     => DIRECTIONALITY_UNDEFINED
      }
  }

  def reverseBytes(ch: scala.Char): scala.Char =
    (((ch >>> 8) & 0xff) | ((ch & 0xff) << 8)).toChar

  private def mapCase(codePoint: Int, f: String => String): Int = {
    if (!isValidCodePoint(codePoint)) codePoint
    else {
      val mapped = f(toString(codePoint))
      if (mapped.length() == 1) PyBuiltins.ord_of(mapped)
      else codePoint
    }
  }

  private def mapCategory(category: String): scala.Byte = category match {
    case "Lu" => UPPERCASE_LETTER
    case "Ll" => LOWERCASE_LETTER
    case "Lt" => TITLECASE_LETTER
    case "Lm" => MODIFIER_LETTER
    case "Lo" => OTHER_LETTER
    case "Mn" => NON_SPACING_MARK
    case "Me" => ENCLOSING_MARK
    case "Mc" => COMBINING_SPACING_MARK
    case "Nd" => DECIMAL_DIGIT_NUMBER
    case "Nl" => LETTER_NUMBER
    case "No" => OTHER_NUMBER
    case "Zs" => SPACE_SEPARATOR
    case "Zl" => LINE_SEPARATOR
    case "Zp" => PARAGRAPH_SEPARATOR
    case "Cc" => CONTROL
    case "Cf" => FORMAT
    case "Co" => PRIVATE_USE
    case "Cs" => SURROGATE
    case "Pd" => DASH_PUNCTUATION
    case "Ps" => START_PUNCTUATION
    case "Pe" => END_PUNCTUATION
    case "Pc" => CONNECTOR_PUNCTUATION
    case "Po" => OTHER_PUNCTUATION
    case "Sm" => MATH_SYMBOL
    case "Sc" => CURRENCY_SYMBOL
    case "Sk" => MODIFIER_SYMBOL
    case "So" => OTHER_SYMBOL
    case "Pi" => INITIAL_QUOTE_PUNCTUATION
    case "Pf" => FINAL_QUOTE_PUNCTUATION
    case _    => UNASSIGNED
  }
}
