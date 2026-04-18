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
  private val NumberClass = PyClassName("java.lang.Number")
  private val IntegerClass = PyClassName("java.lang.Integer")
  private val CharacterClass = PyClassName("java.lang.Character")
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
    NumberClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true
      ),
    IntegerClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(NumberClass),
        javaProvided = true,
        staticMethods = MethodMatcher(simpleNamePrefixes = Set("rotateLeft", "parseInt"))
      ),
    CharacterClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
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
       |# Python-native ops (len, ord, .startswith, etc.). A few calls
       |# don't map 1:1, so these helpers carry the semantics.
       |def _scpy_str_substring(s, *bounds):
       |    if len(bounds) == 1:
       |        return s[bounds[0]:]
       |    return s[bounds[0]:bounds[1]]
       |def _scpy_str_contains(s, t):
       |    return t in s
       |def _scpy_str_isempty(s):
       |    return len(s) == 0
       |def _scpy_str_equals(s, t):
       |    return s == t
       |def _scpy_str_equals_ci(s, t):
       |    return s.lower() == t.lower() if isinstance(t, str) else False
       |def _scpy_str_concat(s, t):
       |    return s + t
       |""".stripMargin
