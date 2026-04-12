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

  private val PredefClass = PyClassName("scala.Predef")
  private val CommandLineParserClass = PyClassName("scala.util.CommandLineParser")
  private val ExceptionClass = PyClassName("java.lang.Exception")
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
        constructors = MethodMatcher(exact = Set(ObjectCtor))
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
       |_scpy_mod_scala_Predef_ = _Predef()
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
       |_scpy_mod_scala_util_CommandLineParser_ = _CommandLineParser()
       |
       |# -- scala.runtime.ModuleSerializationProxy --
       |
       |class ModuleSerializationProxy:
       |    def __init__(self, cls):
       |        self.cls = cls
       |""".stripMargin
