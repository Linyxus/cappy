package java.util

import java.io.{Closeable, Flushable}
import java.lang.{Appendable, StringBuilder}
import scala.python.{PyAny, extern, name, native}

private[util] object FormatterPython:
  @extern("builtins")
  private object Builtins extends PyAny:
    @name("format")
    def pyFormat(value: Any, spec: String): String = native

  def format(value: Any, spec: String): String =
    Builtins.pyFormat(value, spec)

final class Formatter(private var dest: Appendable)
    extends Closeable with Flushable {
  private val buffer = new StringBuilder()
  private var closed = false

  def this() =
    this(null.asInstanceOf[Appendable])

  def format(pattern: String, args: Array[AnyRef]): Formatter = {
    checkNotClosed()

    var patternIndex = 0
    var argIndex = 0
    while patternIndex < pattern.length() do
      val current = pattern.substring(patternIndex, patternIndex + 1)
      if current != "%" then
        appendText(current)
        patternIndex += 1
      else if patternIndex + 1 < pattern.length() &&
          pattern.substring(patternIndex + 1, patternIndex + 2) == "%" then
        appendText("%")
        patternIndex += 2
      else
        val spec = parseSpecifier(pattern, patternIndex + 1)
        if spec.conversion == "n" then
          appendText("\n")
        else
          if argIndex >= args.length then
            throw new MissingFormatArgumentException(spec.raw)
          appendText(formatArgument(spec, args(argIndex)))
          argIndex += 1
        patternIndex = spec.nextIndex

    this
  }

  override def toString(): String =
    buffer.toString()

  def flush(): Unit = {
    checkNotClosed()
    if dest != null then
      dest match
        case flushable: Flushable => flushable.flush()
        case _                    => ()
  }

  def close(): Unit = {
    if !closed then
      closed = true
      if dest != null then
        dest match
          case closeable: Closeable => closeable.close()
          case _                    => ()
  }

  private def appendText(text: String): Unit = {
    buffer.append(text)
    if dest != null then
      dest.append(text)
  }

  private def checkNotClosed(): Unit =
    if closed then
      throw new FormatterClosedException()

  private def parseSpecifier(pattern: String, startIndex: Int): FormatSpecifier = {
    var index = startIndex
    var leftAlign = false
    var zeroPad = false
    var upperCase = false

    var keepParsingFlags = true
    while keepParsingFlags && index < pattern.length() do
      val flag = pattern.substring(index, index + 1)
      if flag == "-" then
        leftAlign = true
        index += 1
      else if flag == "0" then
        zeroPad = true
        index += 1
      else if flag == "," then
        throw new UnsupportedOperationException("Formatter grouping flag unsupported: %,")
      else if flag == "+" || flag == " " || flag == "(" || flag == "#" then
        throw new UnsupportedOperationException("Formatter flag unsupported: %" + flag)
      else
        keepParsingFlags = false

    val widthStart = index
    while index < pattern.length() && isDigitAt(pattern, index) do
      index += 1
    val width =
      if widthStart == index then -1
      else Integer.parseInt(pattern.substring(widthStart, index))

    var precision = -1
    if index < pattern.length() && pattern.substring(index, index + 1) == "." then
      index += 1
      val precisionStart = index
      while index < pattern.length() && isDigitAt(pattern, index) do
        index += 1
      if precisionStart == index then
        throw new IllegalFormatPrecisionException(-1)
      precision = Integer.parseInt(pattern.substring(precisionStart, index))

    if index >= pattern.length() then
      throw new UnknownFormatConversionException("%")

    val conversion = pattern.substring(index, index + 1)
    if conversion == "t" || conversion == "T" then
      if index + 1 >= pattern.length() then
        throw new UnknownFormatConversionException(conversion)
      throw new UnsupportedOperationException(
        "Formatter conversion pending L4.10: %" + pattern.substring(startIndex, index + 2)
      )

    if conversion == "S" || conversion == "X" then
      upperCase = true

    new FormatSpecifier(
      raw = "%" + pattern.substring(startIndex, index + 1),
      conversion = conversion,
      width = width,
      precision = precision,
      leftAlign = leftAlign,
      zeroPad = zeroPad,
      upperCase = upperCase,
      nextIndex = index + 1
    )
  }

  private def formatArgument(spec: FormatSpecifier, arg: AnyRef): String =
    spec.conversion match
      case "s" | "S" =>
        val rendered = renderString(arg, spec)
        applyWidth(rendered, spec.width, spec.leftAlign, padWithZero = false, numeric = false)

      case "d" =>
        val rendered = renderInteger(arg, 10, spec.upperCase)
        applyWidth(rendered, spec.width, spec.leftAlign, spec.zeroPad, numeric = true)

      case "o" =>
        val rendered = renderInteger(arg, 8, spec.upperCase)
        applyWidth(rendered, spec.width, spec.leftAlign, spec.zeroPad, numeric = true)

      case "x" | "X" =>
        val rendered = renderInteger(arg, 16, spec.upperCase)
        applyWidth(rendered, spec.width, spec.leftAlign, spec.zeroPad, numeric = true)

      case "f" =>
        val rendered = renderFloat(arg, spec.precision)
        applyWidth(rendered, spec.width, spec.leftAlign, spec.zeroPad, numeric = true)

      case "%" =>
        applyWidth("%", spec.width, spec.leftAlign, padWithZero = false, numeric = false)

      case _ =>
        throw new UnsupportedOperationException(
          "Formatter conversion pending L4.10: " + spec.raw
        )

  private def renderString(arg: AnyRef, spec: FormatSpecifier): String = {
    var rendered = String.valueOf(arg)
    if spec.precision >= 0 && spec.precision < rendered.length() then
      rendered = rendered.substring(0, spec.precision)
    if spec.upperCase then
      rendered = rendered.toUpperCase()
    rendered
  }

  private def renderInteger(arg: AnyRef, base: Int, upperCase: Boolean): String = {
    val value = arg.asInstanceOf[Long]
    if base == 10 then
      value.toString()
    else
      val digits =
        if upperCase then "0123456789ABCDEF"
        else "0123456789abcdef"
      if value == 0L then
        "0"
      else
        var remaining = if value < 0L then -value else value
        var out = ""
        while remaining > 0L do
          val digit = (remaining % base.toLong).toInt
          out = digits.substring(digit, digit + 1) + out
          remaining = remaining / base.toLong
        if value < 0L then "-" + out else out
  }

  private def renderFloat(arg: AnyRef, precision: Int): String = {
    val digits = if precision >= 0 then precision else 6
    FormatterPython.format(arg.asInstanceOf[Double], "." + digits + "f")
  }

  private def applyWidth(
      value: String,
      width: Int,
      leftAlign: Boolean,
      padWithZero: Boolean,
      numeric: Boolean
  ): String = {
    if width < 0 || value.length() >= width then
      value
    else
      val padding = repeat(if padWithZero && !leftAlign then "0" else " ", width - value.length())
      if leftAlign then
        value + padding
      else if padWithZero && numeric && value.startsWith("-") then
        "-" + padding + value.substring(1)
      else
        padding + value
  }

  private def repeat(text: String, count: Int): String = {
    var out = ""
    var i = 0
    while i < count do
      out += text
      i += 1
    out
  }

  private def isDigitAt(text: String, index: Int): Boolean = {
    val ch = text.substring(index, index + 1)
    "0123456789".indexOf(ch, 0) >= 0
  }
}

private final class FormatSpecifier(
    val raw: String,
    val conversion: String,
    val width: Int,
    val precision: Int,
    val leftAlign: Boolean,
    val zeroPad: Boolean,
    val upperCase: Boolean,
    val nextIndex: Int
)
