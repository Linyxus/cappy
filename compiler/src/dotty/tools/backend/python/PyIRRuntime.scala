package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

/** Shared runtime contract for the Python backend.
 *
 *  This is the nominal surface that the bundled runtime preamble
 *  provides without a corresponding `PyClassDef` in the input.
 */
object PyIRRuntime:

  private[python] final case class MethodMatcher(
      exact: Set[PyMethodName] = Set.empty,
      simpleNamePrefixes: Set[String] = Set.empty
  ):
    def allows(method: PyMethodName): Boolean =
      exact.contains(method) || simpleNamePrefixes.exists(method.simple.name.startsWith)

  private[python] object MethodMatcher:
    val empty: MethodMatcher = MethodMatcher()

  private[python] final case class ProvidedClass(
      kind:            PyClassKind,
      superClass:      Option[PyClassName],
      interfaces:      List[PyClassName] = Nil,
      fields:          Set[PyFieldName] = Set.empty,
      instanceMethods: MethodMatcher = MethodMatcher.empty,
      staticMethods:   MethodMatcher = MethodMatcher.empty,
      constructors:    MethodMatcher = MethodMatcher.empty,
      /** When true, the linker allows unknown instance methods against
       *  this class (it's backed by Python builtins, not compiled PyIR).
       *  Only Java-provided types should set this. */
      javaProvided:    Boolean = false
  ):
    def hasField(field: PyFieldName): Boolean =
      fields.contains(field)

    def hasInstanceMethod(method: PyMethodName): Boolean =
      instanceMethods.allows(method)

    def hasStaticMethod(method: PyMethodName): Boolean =
      staticMethods.allows(method)

    def hasConstructor(method: PyMethodName): Boolean =
      constructors.allows(method)

  // Java-only class names. These have no Scala source and are backed by
  // Python builtins (object, str, type, annotation base classes).
  private val SerializableClass = PyClassName("java.io.Serializable")
  private val Function0Class = PyClassName("scala.Function0")
  private val Function1Class = PyClassName("scala.Function1")
  private val Function2Class = PyClassName("scala.Function2")
  private val AnnotationClass = PyClassName("scala.annotation.Annotation")
  private val StaticAnnotationClass = PyClassName("scala.annotation.StaticAnnotation")
  private val ComparableClass = PyClassName("java.lang.Comparable")

  // Scala's by-ref closure-capture wrappers. The JVM lowers
  // `var x = 0; ... = { () => x += 1 }` into `val x$1 = new IntRef(0)`
  // plus `x$1.elem += 1`; we mirror that at Python runtime so the
  // capture semantics match without needing a per-call-site facade.
  private val IntRefClass       = PyClassName("scala.runtime.IntRef")
  private val LongRefClass      = PyClassName("scala.runtime.LongRef")
  private val DoubleRefClass    = PyClassName("scala.runtime.DoubleRef")
  private val FloatRefClass     = PyClassName("scala.runtime.FloatRef")
  private val BooleanRefClass   = PyClassName("scala.runtime.BooleanRef")
  private val ByteRefClass      = PyClassName("scala.runtime.ByteRef")
  private val CharRefClass      = PyClassName("scala.runtime.CharRef")
  private val ShortRefClass     = PyClassName("scala.runtime.ShortRef")
  private val ObjectRefClass    = PyClassName("scala.runtime.ObjectRef")
  private val VolatileIntRefClass     = PyClassName("scala.runtime.VolatileIntRef")
  private val VolatileLongRefClass    = PyClassName("scala.runtime.VolatileLongRef")
  private val VolatileDoubleRefClass  = PyClassName("scala.runtime.VolatileDoubleRef")
  private val VolatileFloatRefClass   = PyClassName("scala.runtime.VolatileFloatRef")
  private val VolatileBooleanRefClass = PyClassName("scala.runtime.VolatileBooleanRef")
  private val VolatileByteRefClass    = PyClassName("scala.runtime.VolatileByteRef")
  private val VolatileCharRefClass    = PyClassName("scala.runtime.VolatileCharRef")
  private val VolatileShortRefClass   = PyClassName("scala.runtime.VolatileShortRef")
  private val VolatileObjectRefClass  = PyClassName("scala.runtime.VolatileObjectRef")

  private val ObjectCtor =
    PyMethodName(
      PySimpleMethodName.Constructor,
      Nil,
      PyPrimRef.VoidRef
    )

  /** Java-provided nominal classes.
   *
   *  These are the irreducible core: types with no Scala source that are
   *  backed by Python builtins. Everything else (Predef, Statics,
   *  ScalaRunTime, Product, Equals, MatchError, etc.) is now compiled
   *  from `scala-library-py` into `.pyir` artifacts and loaded from the
   *  classpath at link time.
   *
   *  All entries set `javaProvided = true` so the linker allows unknown
   *  instance methods (Python builtins expose methods via `__getattr__`).
   */
  private[python] val providedClasses: Map[PyClassName, ProvidedClass] = Map(
    PyClassName.ObjectClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = None,
        javaProvided = true,
        constructors = MethodMatcher(exact = Set(ObjectCtor)),
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "toString", "__str__", "hashCode", "__hash__",
            "equals", "__eq__", "getClass", "clone", "finalize",
            "wait", "notify", "notifyAll"
          )
        )
      ),
    PyClassName.StringClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true
      ),
    PyClassName.ClassClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true
      ),
    SerializableClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true
      ),
    Function0Class ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("apply"))
      ),
    Function1Class ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("apply"))
      ),
    Function2Class ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("apply"))
      ),
    AnnotationClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    StaticAnnotationClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(AnnotationClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    ComparableClass ->
      // No source port — keeping our own would clash with dotc's
      // JDK-derived view (StaleSymbolException during typer). The linker
      // is lenient because javaProvided = true, and at Python runtime
      // `compareTo` is called via normal attribute dispatch.
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true
      ),
  ) ++ Seq(
    IntRefClass, LongRefClass, DoubleRefClass, FloatRefClass,
    BooleanRefClass, ByteRefClass, CharRefClass, ShortRefClass,
    ObjectRefClass,
    VolatileIntRefClass, VolatileLongRefClass, VolatileDoubleRefClass,
    VolatileFloatRefClass, VolatileBooleanRefClass, VolatileByteRefClass,
    VolatileCharRefClass, VolatileShortRefClass, VolatileObjectRefClass,
  ).map { name =>
    name -> ProvidedClass(
      kind = PyClassKind.Class,
      superClass = Some(PyClassName.ObjectClass),
      javaProvided = true,
      constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
      staticMethods = MethodMatcher(simpleNamePrefixes = Set("create")),
      fields = Set(PyFieldName(name, PySimpleFieldName("elem")))
    )
  }.toMap

  private[python] def providedClass(className: PyClassName): Option[ProvidedClass] =
    providedClasses.get(className)

  /** Python reserved words that must be escaped in identifiers. */
  val PythonKeywords: Set[String] = Set(
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
    "try", "while", "with", "yield"
  )

  /** Runtime Python source prepended to every bundled output.
   *
   *  Defines numeric-wrapping helpers (`_scpy_i32`, `_scpy_i64`,
   *  `_scpy_f32`), a Scala-faithful `_scpy_to_str`, the `_Predef`
   *  singleton (for `println` etc.), a `_CommandLineParser` stub for
   *  `@main` support, and the `ModuleSerializationProxy` marker.
   *
   *  The `_scpy_to_str` here fixes the semantic bug from the legacy
   *  backend where Python's `str(True)` / `str(None)` would leak
   *  through as `"True"` / `"None"` instead of Scala's `"true"` /
   *  `"null"`.
   */
  val content: String =
    """|# Scala.py runtime (generated by the Scala 3 Python backend)
       |#
       |# Names prefixed with _scpy_ are compiler-invented and don't
       |# correspond to Scala source identifiers.
       |#
       |# This preamble contains compiler-intrinsic helpers plus thin stubs
       |# for Java-only types (the exception hierarchy, annotation base
       |# classes, etc.) that compiled Scala code extends.
       |import struct
       |import builtins as _builtins
       |from typing import Any
       |
       |class Annotation:
       |    pass
       |class StaticAnnotation(Annotation):
       |    pass
       |class Comparable:
       |    pass
       |class Serializable:
       |    pass
       |class _scpy_Class:
       |    def __init__(self, name):
       |        self._scpy_name = name
       |
       |    def getName__Ljava_lang_String(self):
       |        return self._scpy_name
       |Class = _scpy_Class
       |
       |# -- scala.FunctionN wrapper --
       |# `PyClosure` emits `_scpy_Fn(lambda params: body)`. Callers that
       |# treat the closure as a `scala.Function0/1/N` and invoke its
       |# erased `apply__...` method land in __getattr__, which returns
       |# the underlying lambda; the `()` at the call site then calls it.
       |# Callers that invoke the closure directly via `closure(args)`
       |# hit `__call__` which just forwards.
       |class _scpy_Fn:
       |    __slots__ = ("_fn",)
       |    def __init__(self, fn):
       |        self._fn = fn
       |    def __call__(self, *args):
       |        return self._fn(*args)
       |    def __getattr__(self, name):
       |        if name.startswith("apply"):
       |            return object.__getattribute__(self, "_fn")
       |        raise AttributeError(name)
       |
       |# -- scala.runtime.*Ref --
       |# By-ref capture wrappers. Scala's JVM target lowers mutable-var
       |# closure captures into `new IntRef(0)` + `.elem` reads/writes.
       |# Python's lexical closure doesn't need them, but PyIR still
       |# emits the code that constructs them, so the names must resolve.
       |def _scpy_mk_ref(default):
       |    class _Ref:
       |        def __init__(self, elem=default):
       |            self.elem = elem
       |        @staticmethod
       |        def create__I__Lscala_runtime_IntRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__J__Lscala_runtime_LongRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__D__Lscala_runtime_DoubleRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__F__Lscala_runtime_FloatRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__Z__Lscala_runtime_BooleanRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__B__Lscala_runtime_ByteRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__C__Lscala_runtime_CharRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__S__Lscala_runtime_ShortRef(v): return _Ref(v)
       |        @staticmethod
       |        def create__Ljava_lang_Object__Lscala_runtime_ObjectRef(v): return _Ref(v)
       |    return _Ref
       |IntRef = _scpy_mk_ref(0)
       |LongRef = _scpy_mk_ref(0)
       |DoubleRef = _scpy_mk_ref(0.0)
       |FloatRef = _scpy_mk_ref(0.0)
       |BooleanRef = _scpy_mk_ref(False)
       |ByteRef = _scpy_mk_ref(0)
       |CharRef = _scpy_mk_ref('\\x00')
       |ShortRef = _scpy_mk_ref(0)
       |ObjectRef = _scpy_mk_ref(None)
       |# Module-object aliases. `IntRef.create(...)` in Scala lowers to
       |# a static call `IntRef$.create(...)` which the Python backend
       |# emits as `_scpy_mod_scala_runtime_IntRef_.create...(...)`.
       |# Point those names at the class itself so the `@staticmethod`
       |# factories resolve.
       |_scpy_mod_scala_runtime_IntRef_     = IntRef
       |_scpy_mod_scala_runtime_LongRef_    = LongRef
       |_scpy_mod_scala_runtime_DoubleRef_  = DoubleRef
       |_scpy_mod_scala_runtime_FloatRef_   = FloatRef
       |_scpy_mod_scala_runtime_BooleanRef_ = BooleanRef
       |_scpy_mod_scala_runtime_ByteRef_    = ByteRef
       |_scpy_mod_scala_runtime_CharRef_    = CharRef
       |_scpy_mod_scala_runtime_ShortRef_   = ShortRef
       |_scpy_mod_scala_runtime_ObjectRef_  = ObjectRef
       |_scpy_mod_scala_runtime_VolatileIntRef_     = IntRef
       |_scpy_mod_scala_runtime_VolatileLongRef_    = LongRef
       |_scpy_mod_scala_runtime_VolatileDoubleRef_  = DoubleRef
       |_scpy_mod_scala_runtime_VolatileFloatRef_   = FloatRef
       |_scpy_mod_scala_runtime_VolatileBooleanRef_ = BooleanRef
       |_scpy_mod_scala_runtime_VolatileByteRef_    = ByteRef
       |_scpy_mod_scala_runtime_VolatileCharRef_    = CharRef
       |_scpy_mod_scala_runtime_VolatileShortRef_   = ShortRef
       |_scpy_mod_scala_runtime_VolatileObjectRef_  = ObjectRef
       |# Volatile variants collapse to the same class under single-threaded
       |# Python; the distinction is only meaningful on the JVM.
       |VolatileIntRef = IntRef
       |VolatileLongRef = LongRef
       |VolatileDoubleRef = DoubleRef
       |VolatileFloatRef = FloatRef
       |VolatileBooleanRef = BooleanRef
       |VolatileByteRef = ByteRef
       |VolatileCharRef = CharRef
       |VolatileShortRef = ShortRef
       |VolatileObjectRef = ObjectRef
       |
       |# -- Compiler-invented: numeric wrapping (Scala overflow semantics) --
       |
       |def _scpy_i32(x):
       |    return ((_builtins.int(x) + 0x80000000) & 0xFFFFFFFF) - 0x80000000
       |
       |def _scpy_i64(x):
       |    return ((_builtins.int(x) + 0x8000000000000000) & 0xFFFFFFFFFFFFFFFF) - 0x8000000000000000
       |
       |def _scpy_f32(x):
       |    return struct.unpack('f', struct.pack('f', _builtins.float(x)))[0]
       |
       |def _scpy_to_str(x):
       |    # Scala-faithful stringification: matches `String.valueOf`
       |    if x is None:
       |        return "null"
       |    if x is True:
       |        return "true"
       |    if x is False:
       |        return "false"
       |    return _builtins.str(x)
       |
       |# -- java.lang.String helpers --
       |# The compiler lowers `s.method(...)` on `java.lang.String` to
       |# Python-native ops (len, slicing, str methods, bytes.encode, ...).
       |# Where Java and Python differ we centralize the semantics here.
       |def _scpy_unsupported(name):
       |    raise UnsupportedOperationException(name, None)
       |
       |def _scpy_str_required_text(x):
       |    if x is None:
       |        raise NullPointerException()
       |    if isinstance(x, str):
       |        return x
       |    if isinstance(x, int) and not isinstance(x, bool):
       |        return chr(x)
       |    return _builtins.str(x)
       |
       |def _scpy_str_check_index(s, index):
       |    if index < 0 or index >= len(s):
       |        raise StringIndexOutOfBoundsException(index)
       |
       |def _scpy_str_check_inclusive_index(s, index):
       |    if index < 0 or index > len(s):
       |        raise StringIndexOutOfBoundsException(index)
       |
       |def _scpy_chr_is_whitespace(ch):
       |    return ch.isspace()
       |
       |def _scpy_str_char_at(s, index):
       |    _scpy_str_check_index(s, index)
       |    return ord(s[index])
       |
       |def _scpy_str_code_point_at(s, index):
       |    _scpy_str_check_index(s, index)
       |    return ord(s[index])
       |
       |def _scpy_str_code_point_before(s, index):
       |    if index <= 0 or index > len(s):
       |        raise StringIndexOutOfBoundsException(index)
       |    return ord(s[index - 1])
       |
       |def _scpy_str_code_point_count(s, begin, end):
       |    if begin < 0 or end < begin or end > len(s):
       |        raise StringIndexOutOfBoundsException(end if end < begin or end > len(s) else begin)
       |    return end - begin
       |
       |def _scpy_str_offset_by_code_points(s, index, offset):
       |    _scpy_str_check_inclusive_index(s, index)
       |    target = index + offset
       |    if target < 0 or target > len(s):
       |        raise StringIndexOutOfBoundsException(target)
       |    return target
       |
       |def _scpy_str_hash_code(s):
       |    h = 0
       |    for ch in s:
       |        h = _scpy_i32(h * 31 + ord(ch))
       |    return h
       |
       |def _scpy_str_equals(s, t):
       |    return s == t if isinstance(t, str) else False
       |
       |def _scpy_str_equals_ci(s, t):
       |    return s.lower() == t.lower() if isinstance(t, str) else False
       |
       |def _scpy_str_compare_to(s, t):
       |    t = _scpy_str_required_text(t)
       |    limit = len(s) if len(s) < len(t) else len(t)
       |    i = 0
       |    while i < limit:
       |        diff = ord(s[i]) - ord(t[i])
       |        if diff != 0:
       |            return diff
       |        i += 1
       |    return len(s) - len(t)
       |
       |def _scpy_str_compare_to_ci(s, t):
       |    t = _scpy_str_required_text(t)
       |    return _scpy_str_compare_to(s.lower(), t.lower())
       |
       |def _scpy_str_concat(s, t):
       |    return s + _scpy_str_required_text(t)
       |
       |def _scpy_str_substring(s, *bounds):
       |    if len(bounds) == 1:
       |        begin = bounds[0]
       |        end = len(s)
       |    else:
       |        begin, end = bounds
       |    if begin < 0 or end < begin or end > len(s):
       |        bad = begin if begin < 0 or begin > len(s) else end
       |        raise StringIndexOutOfBoundsException(bad)
       |    return s[begin:end]
       |
       |def _scpy_str_contains(s, t):
       |    return _scpy_str_required_text(t) in s
       |
       |def _scpy_str_isempty(s):
       |    return len(s) == 0
       |
       |def _scpy_str_startswith(s, prefix, *offset):
       |    prefix = _scpy_str_required_text(prefix)
       |    if not offset:
       |        return s.startswith(prefix)
       |    toffset = offset[0]
       |    if toffset < 0 or toffset > len(s):
       |        return False
       |    return s.startswith(prefix, toffset)
       |
       |def _scpy_str_index_of(s, target, *rest):
       |    target = _scpy_str_required_text(target)
       |    if not rest:
       |        return s.find(target)
       |    from_index = rest[0]
       |    if from_index < 0:
       |        from_index = 0
       |    if from_index > len(s):
       |        return len(s) if target == '' else -1
       |    return s.find(target, from_index)
       |
       |def _scpy_str_last_index_of(s, target, *rest):
       |    target = _scpy_str_required_text(target)
       |    if not rest:
       |        return s.rfind(target)
       |    from_index = rest[0]
       |    if from_index < 0:
       |        return -1
       |    if target == '':
       |        return from_index if from_index < len(s) else len(s)
       |    start = from_index if from_index < len(s) else len(s) - 1
       |    limit = start + len(target)
       |    return s.rfind(target, 0, limit)
       |
       |def _scpy_str_repeat(s, count):
       |    if count < 0:
       |        raise IllegalArgumentException('count is negative: ' + _builtins.str(count), None)
       |    return s * count
       |
       |def _scpy_str_to_char_array(s):
       |    return [ord(ch) for ch in s]
       |
       |def _scpy_str_get_chars(s, src_begin, src_end, dst, dst_begin):
       |    if src_begin < 0 or src_end < src_begin or src_end > len(s):
       |        raise StringIndexOutOfBoundsException(src_begin if src_begin < 0 else src_end)
       |    count = src_end - src_begin
       |    if dst_begin < 0 or dst_begin + count > len(dst):
       |        raise StringIndexOutOfBoundsException(dst_begin)
       |    i = 0
       |    while i < count:
       |        dst[dst_begin + i] = ord(s[src_begin + i])
       |        i += 1
       |
       |def _scpy_str_trim(s):
       |    start = 0
       |    end = len(s)
       |    while start < end and ord(s[start]) <= 0x20:
       |        start += 1
       |    while end > start and ord(s[end - 1]) <= 0x20:
       |        end -= 1
       |    return s[start:end]
       |
       |def _scpy_str_strip_leading(s):
       |    idx = 0
       |    while idx < len(s) and _scpy_chr_is_whitespace(s[idx]):
       |        idx += 1
       |    return s[idx:]
       |
       |def _scpy_str_strip_trailing(s):
       |    idx = len(s)
       |    while idx > 0 and _scpy_chr_is_whitespace(s[idx - 1]):
       |        idx -= 1
       |    return s[:idx]
       |
       |def _scpy_str_strip(s):
       |    return _scpy_str_strip_trailing(_scpy_str_strip_leading(s))
       |
       |def _scpy_str_is_blank(s):
       |    i = 0
       |    while i < len(s):
       |        if not _scpy_chr_is_whitespace(s[i]):
       |            return False
       |        i += 1
       |    return True
       |
       |def _scpy_str_replace(s, old, new):
       |    return s.replace(_scpy_str_required_text(old), _scpy_str_required_text(new))
       |
       |def _scpy_str_region_matches(s, *args):
       |    if len(args) == 4:
       |        ignore_case = False
       |        toffset, other, ooffset, length = args
       |    else:
       |        ignore_case, toffset, other, ooffset, length = args
       |    other = _scpy_str_required_text(other)
       |    if length < 0 or toffset < 0 or ooffset < 0:
       |        return False
       |    if length > len(s) - toffset or length > len(other) - ooffset:
       |        return False
       |    left = s[toffset:toffset + length]
       |    right = other[ooffset:ooffset + length]
       |    return left.lower() == right.lower() if ignore_case else left == right
       |
       |def _scpy_str_get_bytes_utf8(s):
       |    return _scpy_str_get_bytes(s)
       |
       |def _scpy_str_get_bytes(s, *encoding):
       |    if not encoding:
       |        enc = 'utf-8'
       |    else:
       |        candidate = encoding[0]
       |        if not isinstance(candidate, str):
       |            _scpy_unsupported('java.lang.String.getBytes(Charset) pending L8.1 charset port')
       |        enc = candidate
       |    try:
       |        raw = s.encode(enc)
       |    except LookupError:
       |        _scpy_unsupported('java.lang.String.getBytes unsupported charset: ' + enc)
       |    return [b - 256 if b >= 128 else b for b in raw]
       |
       |def _scpy_str_from_chars(values, offset, count):
       |    end = offset + count
       |    if offset < 0 or count < 0 or end > len(values):
       |        raise StringIndexOutOfBoundsException(offset if offset < 0 else end)
       |    out = []
       |    i = offset
       |    while i < end:
       |        out.append(chr(values[i]))
       |        i += 1
       |    return ''.join(out)
       |
       |def _scpy_str_from_code_points(values, offset, count):
       |    end = offset + count
       |    if offset < 0 or count < 0 or end > len(values):
       |        raise StringIndexOutOfBoundsException(offset if offset < 0 else end)
       |    out = []
       |    i = offset
       |    while i < end:
       |        out.append(chr(values[i]))
       |        i += 1
       |    return ''.join(out)
       |
       |def _scpy_str_from_bytes_utf8(values, offset, length):
       |    end = offset + length
       |    if offset < 0 or length < 0 or end > len(values):
       |        raise StringIndexOutOfBoundsException(offset if offset < 0 else end)
       |    raw = _builtins.bytes((values[i] & 0xFF) for i in range(offset, end))
       |    return raw.decode('utf-8')
       |
       |def _scpy_str_split_lines(s):
       |    xs = []
       |    idx = 0
       |    last = 0
       |    while idx < len(s):
       |        ch = s[idx]
       |        if ch == '\n' or ch == '\r':
       |            xs.append(s[last:idx])
       |            if ch == '\r' and idx + 1 < len(s) and s[idx + 1] == '\n':
       |                idx += 1
       |            last = idx + 1
       |        idx += 1
       |    if last != len(s):
       |        xs.append(s[last:])
       |    return xs
       |
       |def _scpy_str_indent(s, n):
       |    xs = _scpy_str_split_lines(s)
       |    out = []
       |    if n < 0:
       |        width = -n
       |        for line in xs:
       |            idx = 0
       |            limit = len(line) if len(line) < width else width
       |            while idx < limit and _scpy_chr_is_whitespace(line[idx]):
       |                idx += 1
       |            out.append(line[idx:] + '\n')
       |    else:
       |        pad = ' ' * n
       |        for line in xs:
       |            out.append(pad + line + '\n')
       |    return ''.join(out)
       |
       |def _scpy_str_strip_indent(s):
       |    if s == '':
       |        return ''
       |    trailing_nl = s[-1] == '\r' or s[-1] == '\n'
       |    xs = _scpy_str_split_lines(s)
       |    min_leading = None
       |    i = 0
       |    while i < len(xs):
       |        line = xs[i]
       |        if i == len(xs) - 1 or not _scpy_str_is_blank(line):
       |            idx = 0
       |            while idx < len(line) and _scpy_chr_is_whitespace(line[idx]):
       |                idx += 1
       |            if min_leading is None or idx < min_leading:
       |                min_leading = idx
       |        i += 1
       |    if trailing_nl or min_leading is None:
       |        min_leading = 0
       |    parts = []
       |    j = 0
       |    while j < len(xs):
       |        line = xs[j]
       |        if not _scpy_str_is_blank(line):
       |            parts.append(_scpy_str_strip_trailing(line[min_leading:]))
       |        else:
       |            parts.append('')
       |        j += 1
       |    result = '\n'.join(parts)
       |    if trailing_nl:
       |        result += '\n'
       |    return result
       |
       |def _scpy_str_translate_escapes(s):
       |    def is_octal_digit(ch):
       |        return '0' <= ch <= '7'
       |    mapping = {
       |        'b': '\b',
       |        't': '\t',
       |        'n': '\n',
       |        'f': '\f',
       |        'r': '\r',
       |        's': ' ',
       |        '"': '"',
       |        "'": "'",
       |        '\\': '\\',
       |    }
       |    i = 0
       |    out = []
       |    while i < len(s):
       |        if s[i] != '\\':
       |            out.append(s[i])
       |            i += 1
       |            continue
       |        if i + 1 >= len(s):
       |            raise IllegalArgumentException('Illegal escape: `\\(end-of-string)`', None)
       |        ch = s[i + 1]
       |        if ch == '\r':
       |            i += 2
       |            if i < len(s) and s[i] == '\n':
       |                i += 1
       |            continue
       |        if ch == '\n':
       |            i += 2
       |            continue
       |        if ch in mapping:
       |            out.append(mapping[ch])
       |            i += 2
       |            continue
       |        if ch == 'u':
       |            if i + 5 >= len(s):
       |                raise IllegalArgumentException('Illegal escape: `\\u`', None)
       |            digits = s[i + 2:i + 6]
       |            try:
       |                out.append(chr(_builtins.int(digits, 16)))
       |            except ValueError:
       |                raise IllegalArgumentException('Illegal escape: `\\u' + digits + '`', None)
       |            i += 6
       |            continue
       |        if is_octal_digit(ch):
       |            if ch <= '3' and i + 3 < len(s) and is_octal_digit(s[i + 2]) and is_octal_digit(s[i + 3]):
       |                code_point = (_builtins.int(ch) - _builtins.int('0')) * 64
       |                code_point += (_builtins.int(s[i + 2]) - _builtins.int('0')) * 8
       |                code_point += _builtins.int(s[i + 3]) - _builtins.int('0')
       |                out.append(chr(code_point))
       |                i += 4
       |                continue
       |            if i + 2 < len(s) and is_octal_digit(s[i + 2]):
       |                code_point = (_builtins.int(ch) - _builtins.int('0')) * 8
       |                code_point += _builtins.int(s[i + 2]) - _builtins.int('0')
       |                out.append(chr(code_point))
       |                i += 3
       |                continue
       |            out.append(chr(_builtins.int(ch) - _builtins.int('0')))
       |            i += 2
       |            continue
       |        raise IllegalArgumentException('Illegal escape: `\\' + ch + '`', None)
       |    return ''.join(out)
       |""".stripMargin
