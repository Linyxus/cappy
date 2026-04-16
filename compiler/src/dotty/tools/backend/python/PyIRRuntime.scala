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
  // Python builtins (object, str, type, Exception hierarchy).
  private val ExceptionClass = PyClassName("java.lang.Exception")
  private val SerializableClass = PyClassName("java.io.Serializable")
  private val NoSuchElementExceptionClass = PyClassName("java.util.NoSuchElementException")
  private val IndexOutOfBoundsExceptionClass = PyClassName("java.lang.IndexOutOfBoundsException")
  private val IllegalArgumentExceptionClass = PyClassName("java.lang.IllegalArgumentException")
  private val AssertionErrorClass = PyClassName("java.lang.AssertionError")
  private val NumberClass = PyClassName("java.lang.Number")
  private val IntegerClass = PyClassName("java.lang.Integer")
  private val CharacterClass = PyClassName("java.lang.Character")
  private val NotImplementedErrorClass = PyClassName("scala.NotImplementedError")
  private val Function0Class = PyClassName("scala.Function0")
  private val Function1Class = PyClassName("scala.Function1")
  private val Function2Class = PyClassName("scala.Function2")
  private val AnnotationClass = PyClassName("scala.annotation.Annotation")
  private val StaticAnnotationClass = PyClassName("scala.annotation.StaticAnnotation")

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
    PyClassName.ThrowableClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    ExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ThrowableClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    PyClassName.RuntimeExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    PyClassName.NullPointerExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    PyClassName.ArithmeticExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    PyClassName.ClassCastExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    SerializableClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true
      ),
    NoSuchElementExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    IndexOutOfBoundsExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    IllegalArgumentExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.RuntimeExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    AssertionErrorClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
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
    NotImplementedErrorClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ExceptionClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
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
       |#
       |# This preamble contains compiler-intrinsic helpers plus thin stubs
       |# for Java-only types (the exception hierarchy, annotation base
       |# classes, etc.) that compiled Scala code extends.
       |import struct
       |import builtins as _builtins
       |from typing import Any
       |
       |# -- Java exception hierarchy stubs --
       |# Python only has Exception; Scala's JVM-rooted hierarchy needs
       |# nominal Python classes so `class MatchError(RuntimeException):`
       |# resolves at Python runtime.
       |class Throwable(Exception):
       |    pass
       |class RuntimeException(Throwable):
       |    pass
       |class NullPointerException(RuntimeException):
       |    pass
       |class ArithmeticException(RuntimeException):
       |    pass
       |class ClassCastException(RuntimeException):
       |    pass
       |class IndexOutOfBoundsException(RuntimeException):
       |    pass
       |class IllegalArgumentException(RuntimeException):
       |    pass
       |class AssertionError(Throwable):
       |    pass
       |class NotImplementedError(Throwable):
       |    pass
       |class NoSuchElementException(RuntimeException):
       |    pass
       |class Annotation:
       |    pass
       |class StaticAnnotation(Annotation):
       |    pass
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
       |""".stripMargin
