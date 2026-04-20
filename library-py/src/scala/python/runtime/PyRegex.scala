package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's third-party `regex` module.
 *
 *  The Java regex port relies on features that stdlib `re` does not
 *  support (`\p{...}`, possessive quantifiers, set intersections,
 *  Java-style named groups). `regex` covers that surface directly,
 *  so callers only need light Java-specific rewrites rather than a
 *  full engine emulation layer.
 */
object PyRegex:
  @extern("regex")
  private object regex extends PyDynamic

  @extern("builtins")
  private[runtime] object builtins extends PyAny:
    @name("list")
    def toList(iter: Any): PyDynamic = native

    @name("len")
    def lengthOf(value: Any): Int = native

    @name("hasattr")
    def hasAttr(obj: Any, name: String): Boolean = native

    @name("getattr")
    def getAttr(obj: Any, name: String): Any = native

  @extern("operator")
  private[runtime] object operator extends PyAny:
    @name("getitem")
    def getItem(container: Any, index: Int): Any = native

  /** Compile a raw Python regex pattern once; reuse via `PyPattern`. */
  def compile(pattern: String): PyPattern =
    wrapPattern(regex.compile(pattern).asInstanceOf[PyDynamic])

  def compileWithFlags(pattern: String, flags: Int): PyPattern =
    wrapPattern(regex.compile(pattern, flags).asInstanceOf[PyDynamic])

  /** Attempt to match `text` from position 0 against `pattern`. `null`
   *  if no match. */
  def firstMatch(pattern: String, text: String): PyMatch | Null =
    val compiled = compile(pattern)
    compiled.firstMatch(text)

  /** Find the first occurrence of `pattern` anywhere in `text`. */
  def search(pattern: String, text: String): PyMatch | Null =
    val compiled = compile(pattern)
    compiled.search(text)

  def sub(pattern: String, replacement: String, text: String): String =
    regex.sub(pattern, replacement, text).asInstanceOf[String]

  def subn(pattern: String, replacement: String, text: String, count: Int): PySubResult =
    wrapSubResult(regex.subn(pattern, replacement, text, count).asInstanceOf[PyDynamic])

  /** Escape `text` so it matches itself literally inside a regex. */
  def escape(text: String): String =
    regex.escape(text).asInstanceOf[String]

  val IgnoreCaseFlag: Int = regex.IGNORECASE.asInstanceOf[Int]
  val MultilineFlag: Int = regex.MULTILINE.asInstanceOf[Int]
  val DotAllFlag: Int = regex.DOTALL.asInstanceOf[Int]
  val VerboseFlag: Int = regex.VERBOSE.asInstanceOf[Int]
  val AsciiFlag: Int = regex.ASCII.asInstanceOf[Int]
  val Version1Flag: Int = regex.VERSION1.asInstanceOf[Int]

  /** Extract the `pos` attribute of a Python `regex.error` exception, if
   *  present and integer-valued. Returns `-1` when unavailable. The
   *  `regex` module exposes the parse offset directly via `error.pos`,
   *  which is more robust than scraping `"at position N"` out of the
   *  message string. */
  def errorPosition(error: Any): Int =
    if error == null then -1
    else if !builtins.hasAttr(error, "pos") then -1
    else
      val raw = builtins.getAttr(error, "pos")
      if raw == null then -1
      else raw.asInstanceOf[Int]

  private[runtime] def wrapPattern(pattern: PyDynamic): PyPattern =
    new PyPattern(pattern, pattern.groups.asInstanceOf[Int])

  private[runtime] def wrapMatch(m: PyDynamic | Null, groupCount: Int): PyMatch | Null =
    if m == null then null else new PyMatch(m.asInstanceOf[PyDynamic], groupCount)

  private[runtime] def wrapSubResult(result: PyDynamic): PySubResult =
    new PySubResult(result)

final class PyPattern private[runtime] (
    private val underlying: PyDynamic,
    private val groupCount0: Int
):
  def groupCount: Int =
    groupCount0

  def firstMatch(text: String): PyMatch | Null =
    PyRegex.wrapMatch(underlying.`match`(text).asInstanceOf[PyDynamic | Null], groupCount0)

  def firstMatch(text: String, startPos: Int): PyMatch | Null =
    PyRegex.wrapMatch(underlying.`match`(text, startPos).asInstanceOf[PyDynamic | Null], groupCount0)

  def fullMatch(text: String): PyMatch | Null =
    PyRegex.wrapMatch(underlying.fullmatch(text).asInstanceOf[PyDynamic | Null], groupCount0)

  def search(text: String): PyMatch | Null =
    PyRegex.wrapMatch(underlying.search(text).asInstanceOf[PyDynamic | Null], groupCount0)

  def search(text: String, startPos: Int): PyMatch | Null =
    PyRegex.wrapMatch(underlying.search(text, startPos).asInstanceOf[PyDynamic | Null], groupCount0)

  def sub(replacement: String, text: String): String =
    underlying.sub(replacement, text).asInstanceOf[String]

  def subn(replacement: String, text: String, count: Int): PySubResult =
    PyRegex.wrapSubResult(underlying.subn(replacement, text, count).asInstanceOf[PyDynamic])

  def findIter(text: String, startPos: Int): PyMatchIter =
    new PyMatchIter(PyRegex.builtins.toList(underlying.finditer(text, startPos)), groupCount0)

final class PyMatch private[runtime] (
    private val underlying: PyDynamic,
    private val groupCount0: Int
):
  def groupCount: Int =
    groupCount0

  /** Full match text. */
  def matched(): String =
    underlying.group(0).asInstanceOf[String]

  /** `group(n)` — 0 is whole match, 1+ are capture groups. */
  def group(n: Int): String | Null =
    underlying.group(n).asInstanceOf[String | Null]

  def groupByName(name: String): String | Null =
    underlying.group(name).asInstanceOf[String | Null]

  /** Start offset of group n in the source string. */
  def start(n: Int): Int =
    underlying.start(n).asInstanceOf[Int]

  def startByName(name: String): Int =
    underlying.start(name).asInstanceOf[Int]

  def end(n: Int): Int =
    underlying.end(n).asInstanceOf[Int]

  def endByName(name: String): Int =
    underlying.end(name).asInstanceOf[Int]

  def index: Int =
    underlying.start(0).asInstanceOf[Int]

final class PyMatchIter private[runtime] (
    private val backing: PyDynamic,
    private val groupCount0: Int
):
  def length: Int =
    PyRegex.builtins.lengthOf(backing)

  def get(index: Int): PyMatch =
    new PyMatch(PyRegex.operator.getItem(backing, index).asInstanceOf[PyDynamic], groupCount0)

final class PySubResult private[runtime] (private val backing: PyDynamic):
  def text: String =
    PyRegex.operator.getItem(backing, 0).asInstanceOf[String]

  def count: Int =
    PyRegex.operator.getItem(backing, 1).asInstanceOf[Int]
