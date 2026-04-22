package java.nio.charset

import scala.annotation.switch

import java.nio.*

abstract class CharsetEncoder protected (
    cs: Charset,
    private val avgBytesPerChar: Float,
    private val maxBytesPerChar0: Float,
    private var replacement0: Array[Byte]
):
  import CharsetEncoder.*

  protected def this(cs: Charset, avgBytesPerChar: Float, maxBytesPerChar: Float) =
    this(cs, avgBytesPerChar, maxBytesPerChar, Array('?'.toByte))

  private var malformedAction = CodingErrorAction.REPORT
  private var unmappableAction = CodingErrorAction.REPORT
  private var status = Init
  private var activeEndOfInput0 = false

  final def charset(): Charset =
    cs

  final def replacement(): Array[Byte] =
    replacement0

  final def replaceWith(newReplacement: Array[Byte]): CharsetEncoder =
    if newReplacement == null || newReplacement.length == 0 ||
        newReplacement.length > maxBytesPerChar() || !isLegalReplacement(newReplacement)
    then
      throw new IllegalArgumentException()
    replacement0 = newReplacement
    implReplaceWith(newReplacement)
    this

  protected def implReplaceWith(newReplacement: Array[Byte]): Unit = ()

  def isLegalReplacement(repl: Array[Byte]): Boolean =
    val decoder = charset().newDecoder()
    val replBuf = ByteBuffer.wrap(repl)

    var outBufSize = 2
    var done = false
    var legal = false
    while !done do
      val result = decoder.decode(replBuf, CharBuffer.allocate(outBufSize), true)
      if result.isOverflow() then
        outBufSize *= 2
      else
        legal = !replBuf.hasRemaining()
        done = true

    legal

  def malformedInputAction(): CodingErrorAction =
    malformedAction

  final def onMalformedInput(newAction: CodingErrorAction): CharsetEncoder =
    if newAction == null then
      throw new IllegalArgumentException("null CodingErrorAction")
    malformedAction = newAction
    implOnMalformedInput(newAction)
    this

  protected def implOnMalformedInput(newAction: CodingErrorAction): Unit = ()

  def unmappableCharacterAction(): CodingErrorAction =
    unmappableAction

  final def onUnmappableCharacter(newAction: CodingErrorAction): CharsetEncoder =
    if newAction == null then
      throw new IllegalArgumentException("null CodingErrorAction")
    unmappableAction = newAction
    implOnUnmappableCharacter(newAction)
    this

  protected def implOnUnmappableCharacter(newAction: CodingErrorAction): Unit = ()

  final def averageBytesPerChar(): Float =
    avgBytesPerChar

  final def maxBytesPerChar(): Float =
    maxBytesPerChar0

  protected final def activeEndOfInput(): Boolean =
    activeEndOfInput0

  final def encode(in: CharBuffer, out: ByteBuffer, endOfInput: Boolean): CoderResult =
    activeEndOfInput0 = endOfInput
    try
      if status == Flushed || (!endOfInput && status == End) then
        throw new IllegalStateException()

      status = if endOfInput then End else Ongoing

      while true do
        val result1 =
          try encodeLoop(in, out)
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
            if out.remaining() < replacement().length then
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

  final def flush(out: ByteBuffer): CoderResult =
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

  protected def implFlush(out: ByteBuffer): CoderResult =
    CoderResult.UNDERFLOW

  final def reset(): CharsetEncoder =
    status = Init
    implReset()
    this

  protected def implReset(): Unit = ()

  protected def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult

  final def encode(in: CharBuffer): ByteBuffer =
    if in.remaining() == 0 then
      ByteBuffer.allocate(0)
    else
      reset()
      val initialSize = (in.remaining() * averageBytesPerChar()).toInt
      var out = ByteBuffer.allocate(initialSize)

      var encoding = true
      while encoding do
        val result = encode(in, out, endOfInput = true)
        if result.isUnderflow() then
          if in.hasRemaining() then throw new AssertionError()
          encoding = false
        else if result.isOverflow() then
          var next: ByteBuffer | Null = null
          if out.capacity() == 0 then
            next = ByteBuffer.allocate(1)
          else
            next = ByteBuffer.allocate(out.capacity() * 2)
            out.flip()
            next.asInstanceOf[ByteBuffer].put(out)
          out = next.asInstanceOf[ByteBuffer]
        else
          result.throwException()
          throw new AssertionError("should not get here")

      var flushing = true
      while flushing do
        val result = flush(out)
        if result.isUnderflow() then
          flushing = false
        else if result.isOverflow() then
          var next: ByteBuffer | Null = null
          if out.capacity() == 0 then
            next = ByteBuffer.allocate(1)
          else
            next = ByteBuffer.allocate(out.capacity() * 2)
            out.flip()
            next.asInstanceOf[ByteBuffer].put(out)
          out = next.asInstanceOf[ByteBuffer]
        else
          result.throwException()
          throw new AssertionError("should not get here")

      out.flip()
      out

object CharsetEncoder:
  private final val Init = 0
  private final val Ongoing = 1
  private final val End = 2
  private final val Flushed = 3
