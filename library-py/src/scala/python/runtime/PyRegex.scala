package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's `re` module. Covers the minimum surface
 *  that backend-adjacent ports (Formatter's format-spec scanner,
 *  future L5.1 `java.util.regex.Pattern`) need: compile, match,
 *  search, sub (with optional limit), and group extraction.
 *
 *  Python regex syntax differs from Java's in meaningful ways
 *  (possessive quantifiers, look-behind fixed-length, named-group
 *  syntax). This facade is a *transport* layer — callers that need
 *  Java-faithful regex semantics must translate the pattern before
 *  handing it here. The current consumer (Formatter's internal
 *  spec-parser) uses Python-style patterns directly.
 */
object PyRegex:
  @extern("re")
  private object re extends PyAny:
    @name("compile")
    def compilePattern(pattern: String): PyDynamic = native

    @name("match")
    def firstMatch(pattern: String, text: String): PyDynamic | Null = native

    @name("search")
    def anyMatch(pattern: String, text: String): PyDynamic | Null = native

    @name("sub")
    def substitute(pattern: String, replacement: String, text: String): String = native

    @name("escape")
    def escape(text: String): String = native

  /** Compile a raw Python regex pattern once; reuse via `PyPattern`. */
  def compile(pattern: String): PyPattern =
    new PyPattern(re.compilePattern(pattern))

  /** Attempt to match `text` from position 0 against `pattern`. `null`
   *  if no match. */
  def firstMatch(pattern: String, text: String): PyMatch | Null =
    val m = re.firstMatch(pattern, text)
    if m == null then null else new PyMatch(m.asInstanceOf[PyDynamic])

  /** Find the first occurrence of `pattern` anywhere in `text`. */
  def search(pattern: String, text: String): PyMatch | Null =
    val m = re.anyMatch(pattern, text)
    if m == null then null else new PyMatch(m.asInstanceOf[PyDynamic])

  def sub(pattern: String, replacement: String, text: String): String =
    re.substitute(pattern, replacement, text)

  /** Escape `text` so it matches itself literally inside a regex. */
  def escape(text: String): String =
    re.escape(text)

final class PyPattern private[runtime] (private val underlying: PyDynamic):
  def firstMatch(text: String): PyMatch | Null =
    val m = underlying.`match`(text)
    if m == null then null else new PyMatch(m.asInstanceOf[PyDynamic])

  def search(text: String): PyMatch | Null =
    val m = underlying.search(text)
    if m == null then null else new PyMatch(m.asInstanceOf[PyDynamic])

  def sub(replacement: String, text: String): String =
    underlying.sub(replacement, text).asInstanceOf[String]

final class PyMatch private[runtime] (private val underlying: PyDynamic):
  /** Full match text. */
  def matched(): String =
    underlying.group(0).asInstanceOf[String]

  /** `group(n)` — 0 is whole match, 1+ are capture groups. */
  def group(n: Int): String | Null =
    underlying.group(n).asInstanceOf[String | Null]

  /** Start offset of group n in the source string. */
  def start(n: Int): Int =
    underlying.start(n).asInstanceOf[Int]

  def end(n: Int): Int =
    underlying.end(n).asInstanceOf[Int]
