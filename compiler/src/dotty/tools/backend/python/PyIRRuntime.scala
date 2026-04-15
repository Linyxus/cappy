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
      constructors:    MethodMatcher = MethodMatcher.empty
  ):
    def hasField(field: PyFieldName): Boolean =
      fields.contains(field)

    def hasInstanceMethod(method: PyMethodName): Boolean =
      instanceMethods.allows(method)

    def hasStaticMethod(method: PyMethodName): Boolean =
      staticMethods.allows(method)

    def hasConstructor(method: PyMethodName): Boolean =
      constructors.allows(method)

  // Module classes encode with a trailing `_` (from the trailing `$` on the
  // raw java class name) to keep them distinct from a same-named companion
  // class. Plain (non-module) classes carry no such suffix.
  private val PredefClass = PyClassName("scala.Predef_")
  private val CommandLineParserClass = PyClassName("scala.util.CommandLineParser_")
  private val ExceptionClass = PyClassName("java.lang.Exception")
  private val MatchErrorClass = PyClassName("scala.MatchError")
  private val EqualsClass = PyClassName("scala.Equals")
  private val ProductClass = PyClassName("scala.Product")
  private val SerializableClass = PyClassName("java.io.Serializable")
  // `scala.runtime.Statics` is a Java-defined module class, so the encoder
  // re-anchors it to its companion `Statics` class — no trailing `_`.
  private val StaticsClass = PyClassName("scala.runtime.Statics")
  // `scala.runtime.ScalaRunTime` is a Scala object whose module class
  // therefore picks up the `_` suffix.
  private val ScalaRunTimeClass = PyClassName("scala.runtime.ScalaRunTime_")
  private val ScalaReflectEnumClass = PyClassName("scala.reflect.Enum")
  private val NoSuchElementExceptionClass = PyClassName("java.util.NoSuchElementException")
  private val IndexOutOfBoundsExceptionClass = PyClassName("java.lang.IndexOutOfBoundsException")
  private val CommandLineParserParseErrorClass = PyClassName("scala.util.CommandLineParser_ParseError")
  private val ModuleSerializationProxyClass = PyClassName("scala.runtime.ModuleSerializationProxy")

  private val ObjectCtor =
    PyMethodName(
      PySimpleMethodName.Constructor,
      Nil,
      PyPrimRef.VoidRef
    )

  private val ModuleSerializationProxyCtor =
    PyMethodName(
      PySimpleMethodName.Constructor,
      List(PyClassRef(PyClassName.ClassClass)),
      PyPrimRef.VoidRef
    )

  /** Runtime-provided nominal classes and their explicit member surface.
   *
   *  Keep this small and deliberate. Anything not listed here must be
   *  emitted by the input `PyIR` bundle or the linker rejects it.
   */
  private[python] val providedClasses: Map[PyClassName, ProvidedClass] = Map(
    PyClassName.ObjectClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = None,
        constructors = MethodMatcher(exact = Set(ObjectCtor)),
        // Allow toString/hashCode/equals/etc on java.lang.Object so the
        // synthetic methods Scala generates on case classes can call
        // through to Object-level fallbacks (`super.toString`, etc.).
        // Encoded names are dunders or signature-mangled so prefix matching
        // is safe enough for the runtime contract.
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
        superClass = Some(PyClassName.ObjectClass)
      ),
    PyClassName.ClassClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass)
      ),
    PyClassName.ThrowableClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass)
      ),
    ExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ThrowableClass)
      ),
    PyClassName.RuntimeExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ExceptionClass)
      ),
    PyClassName.NullPointerExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass)
      ),
    PyClassName.ArithmeticExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass)
      ),
    PyClassName.ClassCastExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass)
      ),
    PredefClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        staticMethods = MethodMatcher(
          simpleNamePrefixes = Set("println", "print", "assert", "require", "identity", "locally")
        )
      ),
    CommandLineParserClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        staticMethods = MethodMatcher(
          simpleNamePrefixes = Set("showError")
        )
      ),
    CommandLineParserParseErrorClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ExceptionClass)
      ),
    ModuleSerializationProxyClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        constructors = MethodMatcher(exact = Set(ModuleSerializationProxyCtor))
      ),
    MatchErrorClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        constructors = MethodMatcher(
          // Synthesised in pattern-match lowering with a single Object scrutinee
          simpleNamePrefixes = Set("<init>")
        )
      ),
    EqualsClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None
      ),
    ProductClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "productArity", "productPrefix", "productElement",
            "productElementName", "productElementNames", "productIterator",
            "canEqual"
          )
        )
      ),
    SerializableClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None
      ),
    StaticsClass ->
      ProvidedClass(
        // Java-defined: encodes as a regular Class (the encoder re-anchors
        // the module class to its companion class).
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        staticMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "mix", "mixLast", "finalizeHash", "anyHash", "longHash",
            "doubleHash", "floatHash", "ioobe"
          )
        )
      ),
    ScalaReflectEnumClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None
      ),
    NoSuchElementExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    IndexOutOfBoundsExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    ScalaRunTimeClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        staticMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "_toString", "_hashCode", "_equals", "hash", "array_apply",
            "array_update", "array_length", "wrapRefArray", "wrapIntArray",
            "wrapLongArray", "wrapDoubleArray", "wrapFloatArray",
            "wrapBooleanArray", "wrapByteArray", "wrapCharArray", "wrapShortArray"
          )
        )
      )
  )

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
       |import struct
       |import builtins as _builtins
       |from typing import Any
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
       |# -- scala.Predef --
       |#
       |# The compiler emits signature-encoded method names like
       |# `println__Ljava_lang_Object__V`. We route them through
       |# `__getattr__` prefix dispatch so the runtime doesn't need
       |# to enumerate every overload.
       |
       |class _Predef:
       |    def __getattr__(self, name):
       |        if name.startswith("println"):
       |            return lambda *args: _builtins.print(*args)
       |        if name.startswith("print"):
       |            return lambda *args: _builtins.print(*args, end="")
       |        if name.startswith("assert"):
       |            def _assert(cond, msg=None):
       |                if msg is not None:
       |                    assert cond, msg
       |                else:
       |                    assert cond
       |            return _assert
       |        if name.startswith("require"):
       |            def _require(cond, msg=None):
       |                if not cond:
       |                    raise ValueError(msg if msg else "requirement failed")
       |            return _require
       |        if name.startswith("identity") or name.startswith("locally"):
       |            return lambda x: x
       |        raise AttributeError(name)
       |
       |# Module-class instance for `scala.Predef$` (encoded as `Predef_`):
       |_scpy_mod_scala_Predef__ = _Predef()
       |
       |# -- scala.util.CommandLineParser (@main wrapper support) --
       |
       |class CommandLineParser_ParseError(Exception):
       |    pass
       |
       |class _CommandLineParser:
       |    def __getattr__(self, name):
       |        if name.startswith("showError"):
       |            return lambda error: _builtins.print(_builtins.str(error))
       |        raise AttributeError(name)
       |
       |_scpy_mod_scala_util_CommandLineParser__ = _CommandLineParser()
       |
       |# -- scala.runtime.ModuleSerializationProxy --
       |
       |class ModuleSerializationProxy:
       |    def __init__(self, cls):
       |        self.cls = cls
       |
       |# -- scala.MatchError --
       |
       |class MatchError(Exception):
       |    def __init__(self, obj):
       |        super().__init__(_scpy_to_str(obj))
       |
       |# -- scala.runtime.Statics (case-class hashCode helpers) --
       |#
       |# Java-defined: the encoder strips the `$` from the module class and
       |# re-anchors to the companion class `Statics`, so call sites land on
       |# `Statics.mix(...)` (via classIdentifier), not on a `_scpy_mod_*`
       |# variable.
       |
       |class _Statics:
       |    def __getattr__(self, name):
       |        if name.startswith("mix"):
       |            return lambda acc, value: _scpy_i32(acc * 31 + (0 if value is None else hash(value)))
       |        if name.startswith("finalizeHash"):
       |            return lambda hash, length: _scpy_i32(hash ^ length)
       |        if name.startswith("anyHash"):
       |            return lambda x: 0 if x is None else _scpy_i32(hash(x))
       |        if name.startswith("longHash") or name.startswith("doubleHash") or name.startswith("floatHash"):
       |            return lambda x: 0 if x is None else _scpy_i32(hash(x))
       |        raise AttributeError(name)
       |
       |# The PyApplyStatic emitter always routes through `_scpy_mod_*` even
       |# for Java static helpers, so we expose the singleton under that name.
       |_scpy_mod_scala_runtime_Statics_ = _Statics()
       |
       |# -- scala.runtime.ScalaRunTime --
       |
       |class _ScalaRunTime:
       |    def __getattr__(self, name):
       |        if name.startswith("_toString"):
       |            return lambda obj: _scpy_to_str(obj)
       |        if name.startswith("_hashCode"):
       |            return lambda obj: 0 if obj is None else _scpy_i32(hash(obj))
       |        if name.startswith("_equals"):
       |            return lambda a, b: a == b
       |        if name.startswith("hash"):
       |            return lambda x: 0 if x is None else _scpy_i32(hash(x))
       |        raise AttributeError(name)
       |
       |ScalaRunTime_ = _ScalaRunTime()
       |_scpy_mod_scala_runtime_ScalaRunTime__ = ScalaRunTime_
       |""".stripMargin
