/*
 * Companion-only java.lang.String port for the ScalaPy backend.
 *
 * The file path follows the Scala.js javalib convention (`_String.scala`),
 * The runtime class itself is Python `str`, so we only provide the static /
 * factory surface here and let the backend reroute `String.*` calls to this
 * companion explicitly.
 */

package java.lang

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.Comparator
import java.util.regex.Pattern
import scala.python.runtime.PyBuiltins

object _String:
  final val CASE_INSENSITIVE_ORDER: Comparator[String] =
    new Comparator[String] with java.io.Serializable:
      def compare(o1: String, o2: String): Int =
        o1.compareToIgnoreCase(o2)

  def `new`(): String =
    ""

  def `new`(value: Array[Char]): String =
    `new`(value, 0, value.length)

  def `new`(value: Array[Char], offset: Int, count: Int): String =
    checkBoundsForNewFromArray(offset, count, value.length)
    PyBuiltins.string_from_chars(value, offset, count)

  def `new`(bytes: Array[scala.Byte]): String =
    decodeBytes(bytes, Charset.forName("UTF-8"))

  def `new`(bytes: Array[scala.Byte], charsetName: String): String =
    decodeBytes(bytes, Charset.forName(normalizeCharsetName(charsetName)))

  def `new`(bytes: Array[scala.Byte], charset: Charset): String =
    decodeBytes(bytes, ThrowablesSupport.requireNonNull(charset))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int): String =
    `new`(sliceBytes(bytes, offset, length))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int, charsetName: String): String =
    decodeBytes(sliceBytes(bytes, offset, length), Charset.forName(normalizeCharsetName(charsetName)))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int, charset: Charset): String =
    decodeBytes(sliceBytes(bytes, offset, length), ThrowablesSupport.requireNonNull(charset))

  def `new`(codePoints: Array[Int], offset: Int, count: Int): String =
    val end = checkBoundsForNewFromArray(offset, count, codePoints.length)
    var out = ""
    var i = offset
    while i < end do
      out += Character.toString(codePoints(i))
      i += 1
    out

  def `new`(original: String): String =
    ThrowablesSupport.requireNonNull(original)

  def `new`(builder: StringBuilder): String =
    ThrowablesSupport.requireNonNull(builder).toString()

  def `new`(buffer: StringBuffer): String =
    ThrowablesSupport.requireNonNull(buffer).toString()

  def valueOf(b: scala.Boolean): String =
    if b then "true" else "false"

  def valueOf(c: scala.Char): String =
    Character.toString(c)
  def valueOf(i: scala.Int): String = i.toString
  def valueOf(l: scala.Long): String = l.toString
  def valueOf(f: scala.Float): String = f.toString
  def valueOf(d: scala.Double): String = d.toString

  def valueOf(obj: Object): String =
    if obj == null then "null" else obj.toString()

  def valueOf(data: Array[Char]): String =
    valueOf(data, 0, data.length)

  def valueOf(data: Array[Char], offset: Int, count: Int): String =
    `new`(data, offset, count)

  def copyValueOf(data: Array[Char]): String =
    valueOf(data)

  def copyValueOf(data: Array[Char], offset: Int, count: Int): String =
    valueOf(data, offset, count)

  def format(format: String, args: Array[AnyRef]): String =
    val effectiveArgs = normalizeFormatArgs(args)
    new java.util.Formatter().format(format, effectiveArgs).toString()

  // JDK overload taking a Locale. We ignore the locale (Python's
  // formatter is locale-agnostic) but keep the signature so stdlib's
  // `String.format(locale, fmt, args)` calls link.
  def format(l: java.util.Locale, fmt: String, args: Array[AnyRef]): String =
    format(fmt, args)

  def matchesRegex(receiver: String, regex: String): scala.Boolean =
    Pattern.matches(regex, receiver)

  def splitRegex(receiver: String, regex: String): Array[String] =
    Pattern.compile(regex).split(receiver)

  def splitRegex(receiver: String, regex: String, limit: Int): Array[String] =
    Pattern.compile(regex).split(receiver, limit)

  def replaceAllRegex(receiver: String, regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(receiver).replaceAll(replacement)

  def replaceFirstRegex(receiver: String, regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(receiver).replaceFirst(replacement)

  private def normalizeFormatArgs(args: Array[AnyRef]): Array[AnyRef] = {
    if args.length != 1 || args(0) == null then
      args
    else
      try
        val nested = args(0)
        val length = java.lang.reflect.Array.getLength(nested)
        val out = new Array[AnyRef](length)
        var i = 0
        while i < length do
          out(i) = java.lang.reflect.Array.get(nested, i).asInstanceOf[AnyRef]
          i += 1
        out
      catch
        case _: IllegalArgumentException =>
          args
  }

  private def sliceBytes(bytes: Array[scala.Byte], offset: Int, length: Int): Array[scala.Byte] =
    val end = checkBoundsForNewFromArray(offset, length, bytes.length)
    val out = new Array[scala.Byte](length)
    var i = 0
    while i < length do
      out(i) = bytes(offset + i)
      i += 1
    out

  private def normalizeCharsetName(name: String): String =
    Charset.forName(ThrowablesSupport.requireNonNull(name)).name()

  private def decodeBytes(bytes: Array[scala.Byte], charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes)).toString()

  /** Checks bounds and returns `offset + count`, the exclusive end offset. */
  private def checkBoundsForNewFromArray(offset: Int, count: Int, arrayLength: Int): Int =
    val endOffset = offset + count
    if offset < 0 || count < 0 || endOffset < offset || endOffset > arrayLength then
      val detail =
        if offset < 0 || offset > arrayLength then offset
        else if count < 0 then count
        else endOffset - 1
      throw new StringIndexOutOfBoundsException(detail)
    endOffset
