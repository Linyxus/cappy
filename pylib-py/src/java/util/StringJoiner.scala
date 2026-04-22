package java.util

import scala.language.unsafeNulls

final class StringJoiner(delimiter: String, prefix: String, suffix: String)
    extends AnyRef {
  private var emptyValue: String | Null = null
  private var value: String = ""
  private var isEmpty0: Boolean = true

  def this(delimiter: String) =
    this(delimiter, "", "")

  def this(delimiter: CharSequence) =
    this(delimiter.toString(), "", "")

  def this(delimiter: CharSequence, prefix: CharSequence, suffix: CharSequence) =
    this(delimiter.toString(), prefix.toString(), suffix.toString())

  def setEmptyValue(emptyValue: CharSequence): StringJoiner =
    this.emptyValue = emptyValue.toString()
    this

  override def toString(): String =
    if isEmpty0 && emptyValue != null then emptyValue
    else prefix + value + suffix

  def add(newElement: CharSequence): StringJoiner =
    if isEmpty0 then isEmpty0 = false
    else value += delimiter
    value += newElement
    this

  def merge(other: StringJoiner): StringJoiner =
    if !other.isEmpty0 then add(other.value)
    this

  def length(): Int =
    if isEmpty0 && emptyValue != null then emptyValue.length()
    else prefix.length() + value.length() + suffix.length()
}
