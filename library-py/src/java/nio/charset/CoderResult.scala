package java.nio.charset

import scala.annotation.switch

import java.nio.{BufferOverflowException, BufferUnderflowException}
final class CoderResult private (kind: Int, private val inputLength: Int):
  import CoderResult.*

  def isUnderflow(): Boolean = kind == Underflow
  def isOverflow(): Boolean = kind == Overflow
  def isMalformed(): Boolean = kind == Malformed
  def isUnmappable(): Boolean = kind == Unmappable

  def isError(): Boolean =
    isMalformed() || isUnmappable()

  def length(): Int =
    if inputLength < 0 then
      throw new UnsupportedOperationException()
    inputLength

  def throwException(): Unit =
    (kind: @switch) match
      case Underflow  => throw new BufferUnderflowException()
      case Overflow   => throw new BufferOverflowException()
      case Malformed  => throw new MalformedInputException(inputLength)
      case Unmappable => throw new UnmappableCharacterException(inputLength)

object CoderResult:
  private final val Underflow = 0
  private final val Overflow = 1
  private final val Malformed = 2
  private final val Unmappable = 3

  val UNDERFLOW: CoderResult = new CoderResult(Underflow, -1)
  val OVERFLOW: CoderResult = new CoderResult(Overflow, -1)

  private val Malformed1 = new CoderResult(Malformed, 1)
  private val Malformed2 = new CoderResult(Malformed, 2)
  private val Malformed3 = new CoderResult(Malformed, 3)
  private val Malformed4 = new CoderResult(Malformed, 4)

  private val Unmappable1 = new CoderResult(Unmappable, 1)
  private val Unmappable2 = new CoderResult(Unmappable, 2)
  private val Unmappable3 = new CoderResult(Unmappable, 3)
  private val Unmappable4 = new CoderResult(Unmappable, 4)

  def malformedForLength(length: Int): CoderResult =
    validateLength(length)
    (length: @switch) match
      case 1 => Malformed1
      case 2 => Malformed2
      case 3 => Malformed3
      case 4 => Malformed4
      case _ => new CoderResult(Malformed, length)

  def unmappableForLength(length: Int): CoderResult =
    validateLength(length)
    (length: @switch) match
      case 1 => Unmappable1
      case 2 => Unmappable2
      case 3 => Unmappable3
      case 4 => Unmappable4
      case _ => new CoderResult(Unmappable, length)

  private def validateLength(length: Int): Unit =
    if length <= 0 then
      throw new IllegalArgumentException("Non-positive length")
