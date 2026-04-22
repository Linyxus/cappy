package java.nio.charset

import java.lang.Character
import java.nio.{ByteBuffer, CharBuffer}

import scala.python.runtime.{PyCodecs, IncrementalDecoder, IncrementalEncoder}

private[charset] final class PyCodecBackedDecoder(
    cs: Charset,
    private val pythonCodec: String,
    averageCharsPerByte: Float,
    maxCharsPerByte: Float
) extends CharsetDecoder(cs, averageCharsPerByte, maxCharsPerByte):

  private var decoder = newDecoderState()
  private var pendingOutput = ""
  private var pendingResult: CoderResult | Null = null

  protected def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult =
    if pendingOutput.length() != 0 then
      return drainPending(out)

    if pendingResult != null then
      val result = pendingResult.asInstanceOf[CoderResult]
      pendingResult = null
      return result

    if !in.hasRemaining() then
      return CoderResult.UNDERFLOW

    val startPos = in.position()
    val chunk = readRemainingBytes(in)
    val step = decoder.decodeStep(chunk, finalChunk = activeEndOfInput())

    if step.reason == null then
      in.position(startPos + step.consumed)
      val result = emit(step.text, out, CoderResult.UNDERFLOW)
      return result

    if shouldFallbackUtf16ToBigEndian(step.reason) then
      decoder = PyCodecs.newDecoder("utf-16-be")
      val result = decodeLoop(in, out)
      return result

    in.position(startPos + step.errorStart)
    val result = emit(step.text, out, CoderResult.malformedForLength(errorLength(step.errorStart, step.errorEnd)))
    return result

  override protected def implFlush(out: CharBuffer): CoderResult =
    if pendingOutput.length() != 0 then
      drainPending(out)
    else if pendingResult != null then
      val result = pendingResult.asInstanceOf[CoderResult]
      pendingResult = null
      result
    else
      CoderResult.UNDERFLOW

  override protected def implReset(): Unit =
    decoder = newDecoderState()
    pendingOutput = ""
    pendingResult = null

  private def newDecoderState(): IncrementalDecoder =
    PyCodecs.newDecoder(pythonCodec)

  private def shouldFallbackUtf16ToBigEndian(reason: String | Null): Boolean =
    pythonCodec == "utf-16" &&
      reason == "Stream does not start with BOM"

  private def drainPending(out: CharBuffer): CoderResult =
    val writable = Math.min(pendingOutput.length(), out.remaining())
    if writable > 0 then
      out.put(pendingOutput, 0, writable)
    if writable < pendingOutput.length() then
      pendingOutput = pendingOutput.substring(writable)
      CoderResult.OVERFLOW
    else
      pendingOutput = ""
      val result =
        if pendingResult == null then CoderResult.UNDERFLOW
        else pendingResult.asInstanceOf[CoderResult]
      pendingResult = null
      result

  private def emit(text: String, out: CharBuffer, result: CoderResult): CoderResult =
    if text.length() == 0 then
      result
    else
      val writable = Math.min(text.length(), out.remaining())
      if writable > 0 then
        out.put(text, 0, writable)
      if writable < text.length() then
        pendingOutput = text.substring(writable)
        pendingResult = result
        CoderResult.OVERFLOW
      else
        result

private[charset] final class PyCodecBackedEncoder(
    cs: Charset,
    private val pythonCodec: String,
    averageBytesPerChar: Float,
    maxBytesPerChar: Float,
    replacement: Array[Byte] = Array('?'.toByte),
    private val bomPrefix: Array[Byte] = new Array[Byte](0)
) extends CharsetEncoder(cs, averageBytesPerChar, maxBytesPerChar, replacement):

  private var encoder = newEncoderState()
  private var pendingBytes = bomPrefix
  private var pendingIndex = 0
  private var pendingResult: CoderResult | Null = null

  protected def encodeLoop(in: CharBuffer, out: ByteBuffer): CoderResult =
    // Drain any pending bytes first — either a BOM prefix injected at
    // construction / reset, or the overflow tail of a previous emit.
    // For the BOM case there is no stashed result, so fall through to
    // input processing once the drain completes.
    if pendingIndex < pendingBytes.length then
      val remaining = pendingBytes.length - pendingIndex
      val writable = Math.min(remaining, out.remaining())
      if writable > 0 then
        out.put(pendingBytes, pendingIndex, writable)
      pendingIndex += writable
      if pendingIndex < pendingBytes.length then
        return CoderResult.OVERFLOW
      pendingBytes = new Array[Byte](0)
      pendingIndex = 0
      if pendingResult != null then
        val result = pendingResult.asInstanceOf[CoderResult]
        pendingResult = null
        return result

    if !in.hasRemaining() then
      return CoderResult.UNDERFLOW

    val startPos = in.position()
    val text = readRemainingChars(in)
    val safeLength = encodablePrefixLength(text)
    if safeLength == 0 then
      return CoderResult.UNDERFLOW

    val safeText =
      if safeLength == text.length() then text
      else text.substring(0, safeLength)
    val step = encoder.encodeStep(safeText, finalChunk = activeEndOfInput())

    if step.reason == null then
      in.position(startPos + step.consumed)
      val result = emit(step.bytes, out, CoderResult.UNDERFLOW)
      return result

    in.position(startPos + step.errorStart)
    val result = emit(step.bytes, out, classifyEncodeError(safeText, step.errorStart, step.errorEnd))
    return result

  override protected def implFlush(out: ByteBuffer): CoderResult =
    if pendingIndex < pendingBytes.length then
      drainPending(out)
    else if pendingResult != null then
      val result = pendingResult.asInstanceOf[CoderResult]
      pendingResult = null
      result
    else
      CoderResult.UNDERFLOW

  override protected def implReset(): Unit =
    encoder = newEncoderState()
    pendingBytes = bomPrefix
    pendingIndex = 0
    pendingResult = null

  private def newEncoderState(): IncrementalEncoder =
    PyCodecs.newEncoder(pythonCodec)

  private def drainPending(out: ByteBuffer): CoderResult =
    val remaining = pendingBytes.length - pendingIndex
    val writable = Math.min(remaining, out.remaining())
    if writable > 0 then
      out.put(pendingBytes, pendingIndex, writable)
    pendingIndex += writable
    if pendingIndex < pendingBytes.length then
      CoderResult.OVERFLOW
    else
      pendingBytes = new Array[Byte](0)
      pendingIndex = 0
      val result =
        if pendingResult == null then CoderResult.UNDERFLOW
        else pendingResult.asInstanceOf[CoderResult]
      pendingResult = null
      result

  private def emit(bytes: Array[Byte], out: ByteBuffer, result: CoderResult): CoderResult =
    if bytes.length == 0 then
      result
    else
      val writable = Math.min(bytes.length, out.remaining())
      if writable > 0 then
        out.put(bytes, 0, writable)
      if writable < bytes.length then
        pendingBytes = bytes
        pendingIndex = writable
        pendingResult = result
        CoderResult.OVERFLOW
      else
        result

  private def encodablePrefixLength(text: String): Int =
    val len = text.length()
    if len == 0 then 0
    else if Character.isHighSurrogate(text.charAt(len - 1)) then len - 1
    else len

  private def classifyEncodeError(text: String, start: Int, end: Int): CoderResult =
    val length = errorLength(start, end)
    var malformed = false
    var i = start
    while i < end && i < text.length() && !malformed do
      val ch = text.charAt(i)
      if Character.isHighSurrogate(ch) then
        if i + 1 < end && i + 1 < text.length() && Character.isLowSurrogate(text.charAt(i + 1)) then
          i += 2
        else
          malformed = true
      else if Character.isLowSurrogate(ch) then
        malformed = true
      else
        i += 1
    if malformed then CoderResult.malformedForLength(length)
    else CoderResult.unmappableForLength(length)

private def readRemainingBytes(in: ByteBuffer): Array[Byte] =
  val dup = in.duplicate()
  val out = new Array[Byte](dup.remaining())
  dup.get(out)
  out

private def readRemainingChars(in: CharBuffer): String =
  val dup = in.duplicate()
  val out = new Array[Char](dup.remaining())
  var i = 0
  while dup.hasRemaining() do
    out(i) = dup.get()
    i += 1
  new String(out)

private def errorLength(start: Int, end: Int): Int =
  val raw = end - start
  if raw <= 0 then 1 else raw
