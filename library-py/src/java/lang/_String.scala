/*
 * Companion-only java.lang.String port for the ScalaPy backend.
 *
 * The file path follows the Scala.js javalib convention (`_String.scala`),
 * The runtime class itself is Python `str`, so we only provide the static /
 * factory surface here and let the backend reroute `String.*` calls to this
 * companion explicitly.
 */

package java.lang

import java.util.Comparator

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
    val end = checkBoundsForNewFromArray(offset, count, value.length)
    var out = ""
    var i = offset
    while i < end do
      out += value(i)
      i += 1
    out

  def `new`(bytes: Array[scala.Byte]): String =
    PyBuiltins.decode_bytes(bytes, "utf-8")

  def `new`(bytes: Array[scala.Byte], charsetName: String): String =
    PyBuiltins.decode_bytes(bytes, normalizeCharsetName(charsetName))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int): String =
    `new`(sliceBytes(bytes, offset, length))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int, charsetName: String): String =
    PyBuiltins.decode_bytes(sliceBytes(bytes, offset, length), normalizeCharsetName(charsetName))

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
    throw new UnsupportedOperationException("String.format pending L4.10")

  private def sliceBytes(bytes: Array[scala.Byte], offset: Int, length: Int): Array[scala.Byte] =
    val end = checkBoundsForNewFromArray(offset, length, bytes.length)
    val out = new Array[scala.Byte](length)
    var i = 0
    while i < length do
      out(i) = bytes(offset + i)
      i += 1
    out

  private def normalizeCharsetName(name: String): String =
    val nn = ThrowablesSupport.requireNonNull(name)
    nn.toUpperCase() match
      case "UTF-8"      => "utf-8"
      case "US-ASCII"   => "ascii"
      case "ISO-8859-1" => "latin-1"
      case _ =>
        throw new UnsupportedOperationException("String charset pending L8.1: " + nn)

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
