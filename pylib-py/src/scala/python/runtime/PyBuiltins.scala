package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Public wrapper for Python's `builtins` module.
 *
 *  Top-level builtins go through a private `@extern("builtins")`
 *  `PyDynamic` so keyword arguments and Python's overload-like
 *  polymorphism ("int(x)" vs "int(s, base)") just work.
 *
 *  `str` instance methods are exposed via a tiny `PyStr` facade: since
 *  Scala `String` and Python `str` share the same runtime object, we
 *  cast `String.asInstanceOf[PyStr]` at the bridge and invoke the
 *  method directly — no PyDynamic/`__call__` gymnastics.
 */
object PyBuiltins:
  @extern("builtins")
  private object builtins extends PyDynamic

  @extern("builtins", "float")
  private object floatType extends PyDynamic

  @extern("builtins", "bytes")
  private def bytesOf(value: Any): Any = native

  @extern("builtins", "float")
  private class PyFloat extends PyAny:
    def hex(): String = native

  @extern("builtins", "bytes")
  private class PyBytes extends PyAny:
    def decode(encoding: String): String = native

  @extern("operator", "getitem")
  private def getItem(value: Any, key: Any): Any = native

  @extern("builtins", "slice")
  private def sliceOf(start: Int, stop: Int): Any = native

  @extern("operator", "eq")
  private def operatorEq(a: Any, b: Any): Boolean = native

  /** Facade over Python's `str` for method dispatch. Scala `String`
   *  erases to Python `str` so the `asInstanceOf[PyStr]` bridges are
   *  no-ops at runtime. */
  @extern("builtins", "str")
  private class PyStr extends PyAny:
    def isalpha(): Boolean = native
    def isdigit(): Boolean = native
    def isalnum(): Boolean = native
    def isspace(): Boolean = native
    def isupper(): Boolean = native
    def islower(): Boolean = native
    def isascii(): Boolean = native
    def isdecimal(): Boolean = native
    def isnumeric(): Boolean = native
    def isprintable(): Boolean = native
    def upper(): String = native
    def lower(): String = native
    def swapcase(): String = native
    def capitalize(): String = native
    def title(): String = native
    def strip(): String = native
    def lstrip(): String = native
    def rstrip(): String = native
    def encode(encoding: String): PyAny = native
    def join(it: Any): String = native

  private inline def asStr(s: String): PyStr = s.asInstanceOf[PyStr]
  private inline def asFloat(value: Double): PyFloat = value.asInstanceOf[PyFloat]
  private inline def asBytes(value: Any): PyBytes = value.asInstanceOf[PyBytes]

  // --- Numeric formatting -------------------------------------------

  def hex_of(value: Long): String =
    builtins.hex(value).asInstanceOf[String]

  def oct_of(value: Long): String =
    builtins.oct(value).asInstanceOf[String]

  def bin_of(value: Long): String =
    builtins.bin(value).asInstanceOf[String]

  def chr_of(codePoint: Int): String =
    builtins.chr(codePoint).asInstanceOf[String]

  /** Bulk-build a Scala/Python `str` from a slice of an `Array[Char]`.
   *  Compiles to one Scala→Python boundary crossing — the iteration,
   *  per-codepoint `chr`, and `str` concatenation all happen in
   *  CPython's C-level `str.join(map(...))`. The original
   *  `var out = ""; out += chr(c); ...` loop in `String.new(Array[Char])`
   *  was the StringBuilder.toString hot path. */
  def string_from_chars(value: Array[Char], offset: Int, count: Int): String =
    val end = offset + count
    val src: Any =
      if offset == 0 && end == value.length then value
      else getItem(value, sliceOf(offset, end))
    asStr("").join(builtins.map(builtins.chr, src))

  def ord_of(c: String): Int =
    builtins.ord(c).asInstanceOf[Int]

  def equal(a: Any, b: Any): Boolean =
    operatorEq(a, b)

  @extern("builtins", "isinstance")
  private def isInstanceOf(value: Any, ty: Any): Boolean = native

  /** True iff `value` is a Python `float`. Used by `BoxesRunTime.equals`
   *  to detect boxed `Double`/`Float` operands so the IEEE-754
   *  `NaN != NaN` semantics survive the unboxed-primitive identity. */
  def is_float(value: Any): Boolean =
    isInstanceOf(value, floatType)

  // --- Parsing ------------------------------------------------------

  def int_parse(text: String): Long =
    builtins.int(text).asInstanceOf[Long]

  def int_parse(text: String, base: Int): Long =
    builtins.int(text, base).asInstanceOf[Long]

  def float_parse(text: String): Double =
    builtins.float(text).asInstanceOf[Double]

  /** Coerce any Python numeric value to a Python `float`. `math.floor`
   *  and `math.ceil` return `int`, but their Java counterparts return
   *  `double`; route their results through this helper at the boundary
   *  so downstream `Double.toString` and string-concat see a real float. */
  def float_of(value: Any): Double =
    builtins.float(value).asInstanceOf[Double]

  def float_from_hex(text: String): Double =
    floatType.fromhex(text).asInstanceOf[Double]

  def float_hex(value: Double): String =
    asFloat(value).hex()

  // --- Numeric ops (typed variants of Python's polymorphic builtins) -

  def abs_int(x: Int): Int =
    builtins.abs(x).asInstanceOf[Int]

  def abs_long(x: Long): Long =
    builtins.abs(x).asInstanceOf[Long]

  def abs_double(x: Double): Double =
    builtins.abs(x).asInstanceOf[Double]

  def min_int(a: Int, b: Int): Int =
    builtins.min(a, b).asInstanceOf[Int]

  def min_long(a: Long, b: Long): Long =
    builtins.min(a, b).asInstanceOf[Long]

  def min_double(a: Double, b: Double): Double =
    builtins.min(a, b).asInstanceOf[Double]

  def max_int(a: Int, b: Int): Int =
    builtins.max(a, b).asInstanceOf[Int]

  def max_long(a: Long, b: Long): Long =
    builtins.max(a, b).asInstanceOf[Long]

  def max_double(a: Double, b: Double): Double =
    builtins.max(a, b).asInstanceOf[Double]

  /** Python's `round(x)` — banker's (half-to-even) rounding. Returns
   *  int for zero-digit form. Not the same as Java `Math.round`. */
  def round_to_long(x: Double): Long =
    builtins.round(x).asInstanceOf[Long]

  /** Python's `round(x, digits)` — returns float. */
  def round_double(x: Double, digits: Int): Double =
    builtins.round(x, digits).asInstanceOf[Double]

  /** Python's `len(obj)`. Works on str, list, dict, tuple, bytes, etc. */
  def length_of(obj: Any): Int =
    builtins.len(obj).asInstanceOf[Int]

  // --- `str` instance-method helpers --------------------------------

  def is_alpha(s: String): Boolean     = asStr(s).isalpha()
  def is_digit(s: String): Boolean     = asStr(s).isdigit()
  def is_alnum(s: String): Boolean     = asStr(s).isalnum()
  def is_space(s: String): Boolean     = asStr(s).isspace()
  def is_upper(s: String): Boolean     = asStr(s).isupper()
  def is_lower(s: String): Boolean     = asStr(s).islower()
  def is_ascii(s: String): Boolean     = asStr(s).isascii()
  def is_decimal(s: String): Boolean   = asStr(s).isdecimal()
  def is_numeric(s: String): Boolean   = asStr(s).isnumeric()
  def is_printable(s: String): Boolean = asStr(s).isprintable()

  def to_upper(s: String): String   = asStr(s).upper()
  def to_lower(s: String): String   = asStr(s).lower()
  def swap_case(s: String): String  = asStr(s).swapcase()
  def capitalize(s: String): String = asStr(s).capitalize()
  def title_case(s: String): String = asStr(s).title()
  def strip(s: String): String      = asStr(s).strip()
  def lstrip(s: String): String     = asStr(s).lstrip()
  def rstrip(s: String): String     = asStr(s).rstrip()

  /** Python `str.encode(encoding)` returns `bytes`. */
  def encode(s: String, encoding: String): PyAny =
    asStr(s).encode(encoding)

  /** Encode a Scala/Python string to a signed-byte Array[Byte]. */
  def encode_bytes(s: String, encoding: String): Array[Byte] =
    val raw = asBytes(asStr(s).encode(encoding))
    val len = builtins.len(raw).asInstanceOf[Int]
    val out = new Array[Byte](len)
    var i = 0
    while i < len do
      val b = getItem(raw, i).asInstanceOf[Int]
      out(i) = (if b >= 128 then b - 256 else b).toByte
      i += 1
    out

  /** Decode a signed-byte Array[Byte] through Python's `bytes.decode`. */
  def decode_bytes(bytes: Array[Byte], encoding: String): String =
    val unsigned = new Array[Int](bytes.length)
    var i = 0
    while i < bytes.length do
      unsigned(i) = bytes(i) & 0xFF
      i += 1
    asBytes(bytesOf(unsigned)).decode(encoding)

  // --- Class-name introspection -------------------------------------

  /** Top-level prelude helper that returns the user-visible Scala
   *  simple name registered for `obj`'s class. Defined in the bundled
   *  Python prelude (see `PyIRRuntime._scpy_simple_name_of`); reached
   *  from Scala source via the bundle's own `__main__` namespace. */
  @extern("__main__", "_scpy_simple_name_of")
  private def simpleNameOf(obj: Any): String = native

  /** User-visible Scala simple name of `obj`'s registered class.
   *
   *  On the JVM, `getClass.getName.split('$').last` peels off package
   *  and outer-class prefixes to yield the user-written class name
   *  (`D1` for `object Test5 { object D1 extends Enumeration }`). On
   *  the Python backend the `$` separators are mangled to `_` during
   *  class encoding, so JVM-style splitting produces a leaked encoded
   *  name (`Test5_D1_`). This bridges directly to the Scala name that
   *  codegen registered alongside each class in the runtime's class
   *  registry, used by `Enumeration.toString`. */
  def class_simple_name(obj: Any): String =
    simpleNameOf(obj)

  // --- Instance-attribute introspection ----------------------------

  /** Snapshot of `(attrName, attrValue)` pairs for a Scala instance via
   *  Python's `vars(obj)`. Used by `scala.Enumeration.populateNameMap`
   *  to recover declared `val Foo, Bar = Value` names without going
   *  through Java reflection (which our runtime does not implement).
   *  The keys are the unencoded Scala names — Python codegen stores
   *  user-declared `val Red` as `self.Red`, not as the JVM-encoded
   *  method name.
   */
  def instance_attrs(obj: Any): InstanceAttrs =
    val rawDict = builtins.vars(obj)
    new InstanceAttrs(
      builtins.list(rawDict.keys()),
      builtins.list(rawDict.values())
    )

  /** Index-random-access handle over a snapshot of an instance's
   *  attribute names + values. Walked via `length` + `name(i)` /
   *  `value(i)` from Scala source. */
  final class InstanceAttrs private[runtime] (
      private val keyList: Any,
      private val valueList: Any
  ):
    def length: Int =
      builtins.len(keyList).asInstanceOf[Int]

    def name(index: Int): String =
      getItem(keyList, index).asInstanceOf[String]

    def value(index: Int): Any =
      getItem(valueList, index)

  // ============================================================
  //  Public builtins facade
  //  ----------------------
  //  User-facing typed wrappers around Python's `builtins`. The
  //  internal helpers above this point (snake_case, prefixed
  //  with `is_`, `to_`, `_of`, etc.) are compiler/runtime callers
  //  and stay as-is for back-compat. The API below mirrors
  //  Python's `builtins` API surface in Scala camelCase, with
  //  finite arity overloads for variadics and `applyDynamicNamed`
  //  for the few kwarg-only forms (`sorted(reverse=)`, etc.).
  // ============================================================

  // --- I/O and display ----------------------------------------------

  def printLine(): Unit = builtins.print()
  def printLine(a: Any): Unit = builtins.print(a)
  def printLine(a: Any, b: Any): Unit = builtins.print(a, b)
  def printLine(a: Any, b: Any, c: Any): Unit = builtins.print(a, b, c)
  def printLine(a: Any, b: Any, c: Any, d: Any): Unit = builtins.print(a, b, c, d)
  def printLine(a: Any, b: Any, c: Any, d: Any, e: Any): Unit =
    builtins.print(a, b, c, d, e)

  /** Print one value with no trailing newline (Python `print(a, end="")`). */
  def printNoLine(a: Any): Unit =
    builtins.applyDynamicNamed("print")(("", a), ("end", ""))

  /** Print one value with a custom separator and end string. */
  def printWith(a: Any, sep: String, end: String): Unit =
    builtins.applyDynamicNamed("print")(("", a), ("sep", sep), ("end", end))

  def readInput(): String = builtins.input().asInstanceOf[String]
  def readInput(prompt: String): String = builtins.input(prompt).asInstanceOf[String]

  def repr(value: Any): String = builtins.repr(value).asInstanceOf[String]

  def formatValue(value: Any): String = builtins.format(value).asInstanceOf[String]
  def formatValue(value: Any, spec: String): String =
    builtins.format(value, spec).asInstanceOf[String]

  def ascii(value: Any): String = builtins.ascii(value).asInstanceOf[String]

  def dirOf(value: Any): PyAny = builtins.dir(value).asInstanceOf[PyAny]
  def dirOfLocals(): PyAny = builtins.dir().asInstanceOf[PyAny]

  def varsOf(value: Any): PyAny = builtins.vars(value).asInstanceOf[PyAny]
  def varsOfLocals(): PyAny = builtins.vars().asInstanceOf[PyAny]

  def idOf(value: Any): Long = builtins.id(value).asInstanceOf[Long]
  def hashOf(value: Any): Long = builtins.hash(value).asInstanceOf[Long]

  // --- Type-constructor functions -----------------------------------

  def intOf(value: Any): Long = builtins.int(value).asInstanceOf[Long]
  def intOf(text: String, base: Int): Long =
    builtins.int(text, base).asInstanceOf[Long]

  def strOf(value: Any): String = builtins.str(value).asInstanceOf[String]

  def boolOf(value: Any): Boolean = builtins.bool(value).asInstanceOf[Boolean]

  def listOf(it: Any): PyAny = builtins.list(it).asInstanceOf[PyAny]
  def listEmpty(): PyAny = builtins.list().asInstanceOf[PyAny]

  def tupleOf(it: Any): PyAny = builtins.tuple(it).asInstanceOf[PyAny]
  def tupleEmpty(): PyAny = builtins.tuple().asInstanceOf[PyAny]

  def dictOf(it: Any): PyAny = builtins.dict(it).asInstanceOf[PyAny]
  def dictEmpty(): PyAny = builtins.dict().asInstanceOf[PyAny]

  def setOf(it: Any): PyAny = builtins.set(it).asInstanceOf[PyAny]
  def setEmpty(): PyAny = builtins.set().asInstanceOf[PyAny]

  def frozenSetOf(it: Any): PyAny = builtins.frozenset(it).asInstanceOf[PyAny]
  def frozenSetEmpty(): PyAny = builtins.frozenset().asInstanceOf[PyAny]

  def bytearrayOf(it: Any): PyAny = builtins.bytearray(it).asInstanceOf[PyAny]
  def bytearrayEmpty(): PyAny = builtins.bytearray().asInstanceOf[PyAny]

  def complexOf(real: Double): PyAny = builtins.complex(real).asInstanceOf[PyAny]
  def complexOf(real: Double, imag: Double): PyAny =
    builtins.complex(real, imag).asInstanceOf[PyAny]

  def rangeOf(stop: Int): PyAny = builtins.range(stop).asInstanceOf[PyAny]
  def rangeOf(start: Int, stop: Int): PyAny =
    builtins.range(start, stop).asInstanceOf[PyAny]
  def rangeOf(start: Int, stop: Int, step: Int): PyAny =
    builtins.range(start, stop, step).asInstanceOf[PyAny]

  def memoryViewOf(value: Any): PyAny =
    builtins.memoryview(value).asInstanceOf[PyAny]

  def enumerateOf(it: Any): PyAny = builtins.enumerate(it).asInstanceOf[PyAny]
  def enumerateOf(it: Any, start: Int): PyAny =
    builtins.enumerate(it, start).asInstanceOf[PyAny]

  def reversedOf(seq: Any): PyAny = builtins.reversed(seq).asInstanceOf[PyAny]

  def filterOf(f: Any, it: Any): PyAny = builtins.filter(f, it).asInstanceOf[PyAny]

  def mapOf(f: Any, it: Any): PyAny = builtins.map(f, it).asInstanceOf[PyAny]
  def mapOf(f: Any, a: Any, b: Any): PyAny =
    builtins.map(f, a, b).asInstanceOf[PyAny]
  def mapOf(f: Any, a: Any, b: Any, c: Any): PyAny =
    builtins.map(f, a, b, c).asInstanceOf[PyAny]

  def zipOf(a: Any, b: Any): PyAny = builtins.zip(a, b).asInstanceOf[PyAny]
  def zipOf(a: Any, b: Any, c: Any): PyAny =
    builtins.zip(a, b, c).asInstanceOf[PyAny]
  def zipOf(a: Any, b: Any, c: Any, d: Any): PyAny =
    builtins.zip(a, b, c, d).asInstanceOf[PyAny]

  def iterOf(value: Any): PyAny = builtins.iter(value).asInstanceOf[PyAny]
  def iterOf(callable: Any, sentinel: Any): PyAny =
    builtins.iter(callable, sentinel).asInstanceOf[PyAny]

  /** Python `type(obj)`. Routed through `applyDynamic` because `type`
   *  is a Scala soft keyword that cannot appear as a `selectDynamic`
   *  identifier in source. */
  def typeOf(value: Any): PyAny =
    builtins.applyDynamic("type")(value).asInstanceOf[PyAny]

  def newSlice(stop: Int): PyAny = builtins.slice(stop).asInstanceOf[PyAny]
  def newSlice(start: Int, stop: Int, step: Int): PyAny =
    builtins.slice(start, stop, step).asInstanceOf[PyAny]

  // --- Numeric -------------------------------------------------------

  def divMod(a: Long, b: Long): (Long, Long) =
    val pair = builtins.divmod(a, b)
    (getItem(pair, 0).asInstanceOf[Long], getItem(pair, 1).asInstanceOf[Long])

  def divMod(a: Double, b: Double): (Double, Double) =
    val pair = builtins.divmod(a, b)
    (getItem(pair, 0).asInstanceOf[Double], getItem(pair, 1).asInstanceOf[Double])

  def powOf(base: Long, exp: Long): Long =
    builtins.pow(base, exp).asInstanceOf[Long]
  def powOf(base: Long, exp: Long, mod: Long): Long =
    builtins.pow(base, exp, mod).asInstanceOf[Long]
  def powOf(base: Double, exp: Double): Double =
    builtins.pow(base, exp).asInstanceOf[Double]

  def sumOf(it: Any): Long = builtins.sum(it).asInstanceOf[Long]
  def sumOf(it: Any, start: Long): Long = builtins.sum(it, start).asInstanceOf[Long]
  def sumDoubles(it: Any, start: Double): Double =
    builtins.sum(it, start).asInstanceOf[Double]

  // --- Sequence / iteration ------------------------------------------

  /** Public alias for `length_of`. */
  def lenOf(value: Any): Int = length_of(value)

  def sortedOf(it: Any): PyAny = builtins.sorted(it).asInstanceOf[PyAny]
  def sortedDesc(it: Any): PyAny =
    builtins
      .applyDynamicNamed("sorted")(("", it), ("reverse", true))
      .asInstanceOf[PyAny]
  def sortedBy(it: Any, key: Any): PyAny =
    builtins
      .applyDynamicNamed("sorted")(("", it), ("key", key))
      .asInstanceOf[PyAny]
  def sortedByDesc(it: Any, key: Any): PyAny =
    builtins
      .applyDynamicNamed("sorted")(("", it), ("key", key), ("reverse", true))
      .asInstanceOf[PyAny]

  def nextOf(it: Any): PyAny = builtins.next(it).asInstanceOf[PyAny]
  def nextOrElse(it: Any, default: Any): PyAny =
    builtins.next(it, default).asInstanceOf[PyAny]

  def anyOf(it: Any): Boolean = builtins.any(it).asInstanceOf[Boolean]
  def allOf(it: Any): Boolean = builtins.all(it).asInstanceOf[Boolean]

  // --- Attribute access / introspection ------------------------------

  def getAttr(obj: Any, name: String): Any = builtins.getattr(obj, name)
  def getAttrOrElse(obj: Any, name: String, default: Any): Any =
    builtins.getattr(obj, name, default)
  def setAttr(obj: Any, name: String, value: Any): Unit =
    builtins.setattr(obj, name, value)
  def hasAttr(obj: Any, name: String): Boolean =
    builtins.hasattr(obj, name).asInstanceOf[Boolean]
  def delAttr(obj: Any, name: String): Unit =
    builtins.delattr(obj, name)

  /** Public alias of the private `isInstanceOf` helper. */
  def isInstance(value: Any, ty: Any): Boolean = isInstanceOf(value, ty)

  def isSubclass(cls: Any, classinfo: Any): Boolean =
    builtins.issubclass(cls, classinfo).asInstanceOf[Boolean]

  def isCallable(value: Any): Boolean =
    builtins.callable(value).asInstanceOf[Boolean]

  // --- Eval / exec / compile -----------------------------------------

  def evalOf(source: String): Any = builtins.eval(source)
  def execOf(source: String): Unit = builtins.exec(source)
  def compileOf(source: String, filename: String, mode: String): PyAny =
    builtins.compile(source, filename, mode).asInstanceOf[PyAny]
  def globalsOf(): PyAny = builtins.globals().asInstanceOf[PyAny]
  def localsOf(): PyAny = builtins.locals().asInstanceOf[PyAny]

  // --- Misc ----------------------------------------------------------

  /** Python `__import__(name)` — low-level module import. */
  def importModule(moduleName: String): PyDynamic =
    builtins.applyDynamic("__import__")(moduleName).asInstanceOf[PyDynamic]

  // --- Constants -----------------------------------------------------

  @extern("builtins", "Ellipsis")
  private object ellipsisHandle extends PyAny

  @extern("builtins", "NotImplemented")
  private object notImplementedHandle extends PyAny

  def ellipsis: PyAny = ellipsisHandle
  def notImplemented: PyAny = notImplementedHandle

  // --- Exception type facades ----------------------------------------
  //
  // Each class below is an `@extern` reference to the Python builtin
  // exception of the same name. They are type-only handles intended
  // for `isInstance` / `isSubclass` checks and for binding catch-all
  // results — they intentionally extend `PyAny` (not `Throwable`) so
  // they participate in the Python facade type system rather than
  // the JVM exception hierarchy. Catching a specific Python exception
  // type at a Scala `catch` site is not currently supported; user
  // code that wants typed handling should `catch case _: Throwable`
  // and use `isInstance` against these handles.

  @extern("builtins", "BaseException") class BaseException extends PyAny
  @extern("builtins", "Exception") class Exception extends PyAny
  @extern("builtins", "ArithmeticError") class ArithmeticError extends PyAny
  @extern("builtins", "ZeroDivisionError") class ZeroDivisionError extends PyAny
  @extern("builtins", "OverflowError") class OverflowError extends PyAny
  @extern("builtins", "FloatingPointError") class FloatingPointError extends PyAny
  @extern("builtins", "AssertionError") class AssertionError extends PyAny
  @extern("builtins", "AttributeError") class AttributeError extends PyAny
  @extern("builtins", "ValueError") class ValueError extends PyAny
  @extern("builtins", "TypeError") class TypeError extends PyAny
  @extern("builtins", "LookupError") class LookupError extends PyAny
  @extern("builtins", "KeyError") class KeyError extends PyAny
  @extern("builtins", "IndexError") class IndexError extends PyAny
  @extern("builtins", "NameError") class NameError extends PyAny
  @extern("builtins", "RuntimeError") class RuntimeError extends PyAny
  @extern("builtins", "NotImplementedError") class NotImplementedError extends PyAny
  @extern("builtins", "RecursionError") class RecursionError extends PyAny
  @extern("builtins", "ImportError") class ImportError extends PyAny
  @extern("builtins", "ModuleNotFoundError") class ModuleNotFoundError extends PyAny
  @extern("builtins", "OSError") class OSError extends PyAny
  @extern("builtins", "FileNotFoundError") class FileNotFoundError extends PyAny
  @extern("builtins", "MemoryError") class MemoryError extends PyAny
  @extern("builtins", "StopIteration") class StopIteration extends PyAny
  @extern("builtins", "StopAsyncIteration") class StopAsyncIteration extends PyAny
  @extern("builtins", "KeyboardInterrupt") class KeyboardInterrupt extends PyAny
  @extern("builtins", "SystemExit") class SystemExit extends PyAny
  @extern("builtins", "GeneratorExit") class GeneratorExit extends PyAny
  @extern("builtins", "UnicodeError") class UnicodeError extends PyAny
  @extern("builtins", "UnicodeDecodeError") class UnicodeDecodeError extends PyAny
  @extern("builtins", "UnicodeEncodeError") class UnicodeEncodeError extends PyAny
