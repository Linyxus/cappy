package java.nio.charset

import scala.annotation.switch

import java.nio.*

abstract class CharsetDecoder protected (
    cs: Charset,
    private val avgCharsPerByte: Float,
    private val maxCharsPerByte0: Float
):
  import CharsetDecoder.*

  private var replacement0 = "\uFFFD"
  private var malformedAction = CodingErrorAction.REPORT
  private var unmappableAction = CodingErrorAction.REPORT
  private var status = Init
  private var activeEndOfInput0 = false

  final def charset(): Charset =
    cs

  final def replacement(): String =
    replacement0

  final def replaceWith(newReplacement: String): CharsetDecoder =
    if newReplacement == null || newReplacement == "" then
      throw new IllegalArgumentException("Invalid replacement: " + newReplacement)
    if newReplacement.length() > maxCharsPerByte() then
      throw new IllegalArgumentException("Replacement string cannot be longer than maxCharsPerByte")
    replacement0 = newReplacement
    implReplaceWith(newReplacement)
    this

  protected def implReplaceWith(newReplacement: String): Unit = ()

  def malformedInputAction(): CodingErrorAction =
    malformedAction

  final def onMalformedInput(newAction: CodingErrorAction): CharsetDecoder =
    if newAction == null then
      throw new IllegalArgumentException("null CodingErrorAction")
    malformedAction = newAction
    implOnMalformedInput(newAction)
    this

  protected def implOnMalformedInput(newAction: CodingErrorAction): Unit = ()

  def unmappableCharacterAction(): CodingErrorAction =
    unmappableAction

  final def onUnmappableCharacter(newAction: CodingErrorAction): CharsetDecoder =
    if newAction == null then
      throw new IllegalArgumentException("null CodingErrorAction")
    unmappableAction = newAction
    implOnUnmappableCharacter(newAction)
    this

  protected def implOnUnmappableCharacter(newAction: CodingErrorAction): Unit = ()

  final def averageCharsPerByte(): Float =
    avgCharsPerByte

  final def maxCharsPerByte(): Float =
    maxCharsPerByte0

  protected final def activeEndOfInput(): Boolean =
    activeEndOfInput0

  final def decode(in: ByteBuffer, out: CharBuffer, endOfInput: Boolean): CoderResult =
    activeEndOfInput0 = endOfInput
    try
      if status == Flushed || (!endOfInput && status == End) then
        throw new IllegalStateException()

      status = if endOfInput then End else Ongoing

      while true do
        val result1 =
          try decodeLoop(in, out)
          catch
            case ex: BufferOverflowException  => throw new CoderMalfunctionError(ex)
            case ex: BufferUnderflowException => throw new CoderMalfunctionError(ex)

        var result2 = result1
        if result1.isUnderflow() then
          val rem = in.remaining()
          if endOfInput && rem > 0 then
            result2 = CoderResult.malformedForLength(rem)

        if result2.isUnderflow() || result2.isOverflow() then
          return result2

        val action =
          if result2.isUnmappable() then unmappableCharacterAction()
          else malformedInputAction()

        action match
          case CodingErrorAction.REPLACE =>
            if out.remaining() < replacement().length() then
              return CoderResult.OVERFLOW
            out.put(replacement())
            in.position(in.position() + result2.length())
          case CodingErrorAction.REPORT =>
            return result2
          case CodingErrorAction.IGNORE =>
            in.position(in.position() + result2.length())

      CoderResult.UNDERFLOW
    finally
      activeEndOfInput0 = false

  final def flush(out: CharBuffer): CoderResult =
    (status: @switch) match
      case End =>
        val result = implFlush(out)
        if result.isUnderflow() then
          status = Flushed
        result
      case Flushed =>
        CoderResult.UNDERFLOW
      case _ =>
        throw new IllegalStateException()

  protected def implFlush(out: CharBuffer): CoderResult =
    CoderResult.UNDERFLOW

  final def reset(): CharsetDecoder =
    status = Init
    implReset()
    this

  protected def implReset(): Unit = ()

  protected def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult

  final def decode(in: ByteBuffer): CharBuffer =
    reset()
    val initialSize = (in.remaining().toDouble * averageCharsPerByte()).toInt
    var out = CharBuffer.allocate(initialSize)

    var decoding = true
    while decoding do
      val result = decode(in, out, endOfInput = true)
      if result.isUnderflow() then
        if in.hasRemaining() then throw new AssertionError()
        decoding = false
      else if result.isOverflow() then
        var next: CharBuffer | Null = null
        if out.capacity() == 0 then
          next = CharBuffer.allocate(1)
        else
          next = CharBuffer.allocate(out.capacity() * 2)
          out.flip()
          next.asInstanceOf[CharBuffer].put(out)
        out = next.asInstanceOf[CharBuffer]
      else
        result.throwException()
        throw new AssertionError("should not get here")

    var flushing = true
    while flushing do
      val result = flush(out)
      if result.isUnderflow() then
        flushing = false
      else if result.isOverflow() then
        var next: CharBuffer | Null = null
        if out.capacity() == 0 then
          next = CharBuffer.allocate(1)
        else
          next = CharBuffer.allocate(out.capacity() * 2)
          out.flip()
          next.asInstanceOf[CharBuffer].put(out)
        out = next.asInstanceOf[CharBuffer]
      else
        result.throwException()
        throw new AssertionError("should not get here")

    out.flip()
    out

  def isAutoDetecting(): Boolean =
    false

  def isCharsetDetected(): Boolean =
    throw new UnsupportedOperationException()

  def detectedCharset(): Charset =
    throw new UnsupportedOperationException()

object CharsetDecoder:
  private final val Init = 1
  private final val Ongoing = 2
  private final val End = 3
  private final val Flushed = 4
