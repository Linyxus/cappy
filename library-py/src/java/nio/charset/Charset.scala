package java.nio.charset

import java.lang.{String as JString, ThrowablesSupport}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Collections, HashMap, HashSet, TreeMap}

import scala.python.runtime.PyCodecs

abstract class Charset protected (
    canonicalName: String,
    private[charset] val aliasesArray: Array[String]
) extends AnyRef
    with Comparable[Charset]:

  final def name(): String =
    canonicalName

  def displayName(): String =
    name()

  final def aliases(): java.util.Set[String] =
    val set = new HashSet[String]()
    var i = 0
    while i < aliasesArray.length do
      set.add(aliasesArray(i))
      i += 1
    Collections.unmodifiableSet(set)

  override final def equals(that: Any): Boolean =
    that match
      case that: Charset => this.name() == that.name()
      case _             => false

  override final def hashCode(): Int =
    name().hashCode()

  override final def toString(): String =
    name()

  override final def compareTo(that: Charset): Int =
    name().compareToIgnoreCase(that.name())

  def contains(cs: Charset): Boolean

  def newDecoder(): CharsetDecoder
  def newEncoder(): CharsetEncoder

  def canEncode(): Boolean =
    true

  final def decode(bb: ByteBuffer): CharBuffer =
    // Matches JDK: build a fresh decoder with REPLACE on both malformed
    // and unmappable inputs so callers never see a checked exception
    // from the convenience path. Routing through `newDecoder()` honours
    // subclass hooks and any `replaceWith` overrides set on the fresh
    // decoder — the previous direct-codec path bypassed both.
    try
      newDecoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
        .decode(bb)
    catch
      case x: CharacterCodingException => throw new Error(x)

  final def encode(cb: CharBuffer): ByteBuffer =
    try
      newEncoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
        .encode(cb)
    catch
      case x: CharacterCodingException => throw new Error(x)

  final def encode(str: String): ByteBuffer =
    encode(CharBuffer.wrap(str))

object Charset:
  import StandardCharsets.*

  private def builtins =
    Array[Charset](ISO_8859_1, US_ASCII, UTF_16, UTF_16BE, UTF_16LE, UTF_8)

  private def registry =
    val m = new HashMap[String, Charset]()
    var i = 0
    while i < builtins.length do
      val cs = builtins(i)
      m.put(normalizeLookupKey(cs.name()), cs)
      var j = 0
      while j < cs.aliasesArray.length do
        m.put(normalizeLookupKey(cs.aliasesArray(j)), cs)
        j += 1
      i += 1
    m

  private def availableCharsetsResult =
    val m = new TreeMap[String, Charset](JString.CASE_INSENSITIVE_ORDER)
    var i = 0
    while i < builtins.length do
      val cs = builtins(i)
      m.put(cs.name(), cs)
      i += 1
    m

  def defaultCharset(): Charset =
    UTF_8

  def forName(charsetName: String): Charset =
    val raw = ThrowablesSupport.requireNonNull(charsetName)
    val registered = registry.get(normalizeLookupKey(raw))
    if registered != null then registered
    else
      val canonical = PyCodecs.lookupCanonicalNameOrNull(raw)
      if canonical == null then
        throw new UnsupportedCharsetException(raw)
      val builtin = registry.get(normalizeLookupKey(canonical))
      if builtin != null then builtin
      else new PythonCodecCharset(canonical)

  def isSupported(charsetName: String): Boolean =
    try
      forName(charsetName)
      true
    catch
      case _: UnsupportedCharsetException => false

  def availableCharsets(): java.util.SortedMap[String, Charset] =
    availableCharsetsResult

  private def normalizeLookupKey(name: String): String =
    name.toLowerCase()

private final class PythonCodecCharset(private val pythonName: String)
    extends Charset(pythonName, new Array[String](0)):

  def contains(cs: Charset): Boolean =
    this == cs

  def newDecoder(): CharsetDecoder =
    new PyCodecBackedDecoder(this, pythonName, 1.0f, 1.0f)

  def newEncoder(): CharsetEncoder =
    new PyCodecBackedEncoder(this, pythonName, 1.0f, 4.0f)
