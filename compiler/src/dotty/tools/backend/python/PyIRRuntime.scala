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
  private val EnumClass = PyClassName("java.lang.Enum")
  private val ClassLoaderClass = PyClassName("java.lang.ClassLoader")
  private val ClassValueClass = PyClassName("java.lang.ClassValue")
  private val MirrorClass = PyClassName("scala.deriving.Mirror")
  private val MirrorProductClass = PyClassName("scala.deriving.Mirror_Product")
  private val MirrorSumClass = PyClassName("scala.deriving.Mirror_Sum")
  private val MirrorSingletonClass = PyClassName("scala.deriving.Mirror_Singleton")
  private val MirrorSingletonProxyClass = PyClassName("scala.deriving.Mirror_SingletonProxy")

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
  private val BoxedUnitClass = PyClassName("scala.runtime.BoxedUnit")
  private val IntCompanionClass = PyClassName("scala.Int_")
  private val CharCompanionClass = PyClassName("scala.Char_")

  // VarHandle / MethodHandles / MethodHandles$Lookup — referenced by
  // stdlib's atomic/concurrent specializations. We don't shadow them in
  // pylib because declaring `class MethodHandles { class Lookup }`
  // trips the typer's inner-class resolver while loading
  // `java.lang.String.sig` (which references `MethodHandles$Lookup` in
  // its own signatures). Treat them as javaProvided runtime-baseline so
  // the linker accepts any signature; they're never reached at runtime
  // in pos-py tests.
  private val VarHandleClass         = PyClassName("java.lang.invoke.VarHandle")
  private val MethodHandlesClass     = PyClassName("java.lang.invoke.MethodHandles")
  private val MethodHandlesLookupClass = PyClassName("java.lang.invoke.MethodHandles_Lookup")

  // JDK / scalalib classes referenced by stdlib but not implemented in
  // pylib. Same "linker-only stub" treatment as the invoke classes
  // above. None are exercised at runtime by pos-py tests.
  private val ObjectInputStreamClass     = PyClassName("java.io.ObjectInputStream")
  private val ObjectOutputStreamClass    = PyClassName("java.io.ObjectOutputStream")
  private val AbstractStringBuilderClass = PyClassName("java.lang.AbstractStringBuilder")
  private val ReflectMethodClass         = PyClassName("java.lang.reflect.Method")
  private val ReflectFieldClass          = PyClassName("java.lang.reflect.Field")
  private val ReflectAccessibleObjClass  = PyClassName("java.lang.reflect.AccessibleObject")
  private val SpliteratorClass           = PyClassName("java.util.Spliterator")
  private val RefReferenceClass          = PyClassName("java.lang.ref.Reference")
  private val RefWeakReferenceClass      = PyClassName("java.lang.ref.WeakReference")
  private val ScalaNumberClass           = PyClassName("scala.math.ScalaNumber")
  private val PrimitiveIteratorClass     = PyClassName("java.util.PrimitiveIterator")
  private val PrimitiveIteratorOfIntClass    = PyClassName("java.util.PrimitiveIterator_OfInt")
  private val PrimitiveIteratorOfLongClass   = PyClassName("java.util.PrimitiveIterator_OfLong")
  private val PrimitiveIteratorOfDoubleClass = PyClassName("java.util.PrimitiveIterator_OfDouble")

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
        javaProvided = true,
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "getName", "toString",
            "getSuperclass", "getInterfaces", "getComponentType",
            "isPrimitive", "isInterface", "isArray",
            "isInstance", "isAssignableFrom",
            "getClassLoader",
            "getDeclaredMethods", "getDeclaredFields", "getMethods", "getFields",
            "getDeclaredMethod", "getDeclaredField", "getMethod", "getField"
          )
        ),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set("forName"))
      ),
    ClassLoaderClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("getParent"))
      ),
    ClassValueClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set("get", "remove", "computeValue")
        )
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
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("apply", "toString", "__str__"))
      ),
    Function1Class ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        // `compose`, `andThen`, `toString` are instance methods
        // stdlib references. Runtime Function1 is a Python callable; missing
        // attributes get sane defaults via Python's attribute lookup, so
        // declaring them here only costs a linker-side match.
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set("apply", "compose", "andThen", "toString", "__str__")
        )
      ),
    Function2Class ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set("apply", "curried", "tupled", "toString", "__str__")
        )
      ),
    AnnotationClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    BoxedUnitClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        fields = Set(
          PyFieldName(BoxedUnitClass, PySimpleFieldName("UNIT")),
          PyFieldName(BoxedUnitClass, PySimpleFieldName("TYPE"))
        )
      ),
    IntCompanionClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("toChar")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set("toChar"))
      ),
    CharCompanionClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("toInt")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set("toInt"))
      ),
    StaticAnnotationClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(AnnotationClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>"))
      ),
    VarHandleClass ->
      // Stdlib's atomic specializations reference VarHandle for
      // memory-fence semantics. Empty matcher with javaProvided=true
      // accepts any signature.
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),  // accept all
        staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    MethodHandlesClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    MethodHandlesLookupClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ObjectInputStreamClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ObjectOutputStreamClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    AbstractStringBuilderClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectAccessibleObjClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectMethodClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ReflectAccessibleObjClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectFieldClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ReflectAccessibleObjClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    SpliteratorClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    RefReferenceClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    RefWeakReferenceClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(RefReferenceClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaNumberClass ->
      // Java-defined in stdlib (`scala/math/ScalaNumber.java`) but our
      // pylib pipeline doesn't compile .java files — so the linker would
      // see it missing. Treat as javaProvided.
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    PrimitiveIteratorClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    PrimitiveIteratorOfIntClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(PrimitiveIteratorClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    PrimitiveIteratorOfLongClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(PrimitiveIteratorClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    PrimitiveIteratorOfDoubleClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(PrimitiveIteratorClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
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
    EnumClass ->
      // Like Comparable, `java.lang.Enum` must stay compiler/JDK-owned:
      // dotc's `CompleteJavaEnums` phase assumes the class symbol comes
      // from the JDK classfile loader and crashes if we source-port it.
      // Provide the runtime shape here instead.
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        interfaces = List(ComparableClass, SerializableClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set(
            "name", "ordinal", "toString",
            "compareTo", "clone", "finalize"
          )
        )
      ),
    MirrorClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true
      ),
    MirrorProductClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(MirrorClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("fromProduct"))
      ),
    MirrorSumClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(MirrorClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("ordinal"))
      ),
    MirrorSingletonClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(MirrorProductClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("fromProduct"))
      ),
    MirrorSingletonProxyClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        interfaces = List(MirrorProductClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("fromProduct"))
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

  /** Static edges into Scala-defined stdlib methods that the runtime
   *  [[prelude]] invokes directly. These call sites are not part of any
   *  PyIR tree, so [[PyReachability]] cannot discover them by walking
   *  method bodies. Seed them explicitly so method-level DCE does not
   *  prune the targets.
   *
   *  Each entry is `(module-class name, target method name)`. The class
   *  is treated as instantiated (module singleton) and the method as
   *  reachable.
   */
  private[python] val preludeCalls: List[(PyClassName, PyMethodName)] = List(
    // `_scpy_require_monitor` (see `prelude`) routes Object.wait/notify
    // monitor-state errors through this helper so the exception class is
    // kept live for test cases that don't otherwise mention it.
    PyClassName("java.lang.ThrowablesSupport_")
      -> PyMethodName.noArgs("throwIllegalMonitorState"),
    // `_scpy_str_get_bytes` uses a `hasattr` probe to route through
    // `Charset.encode(String)` when the caller passes a Charset. The
    // call site is reflective, so the analyzer cannot see it; if DCE
    // prunes `encode(String)` then `String.getBytes(charset)` silently
    // falls back to Python's platform-endian codec and UTF-16 output
    // ships with the wrong BOM.
    PyClassName("java.nio.charset.Charset") -> PyMethodName(
      PySimpleMethodName("encode"),
      List(PyClassRef(PyClassName("java.lang.String"))),
      PyClassRef(PyClassName("java.nio.ByteBuffer"))
    )
  )

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
       |import decimal as _scpy_decimal
       |import math as _scpy_math
       |import threading as _scpy_threading
       |import struct
       |import weakref as _scpy_weakref
       |import builtins as _builtins
       |from typing import Any
       |
       |_scpy_len = _builtins.len
       |_scpy_monitor_bootstrap = _scpy_threading.RLock()
       |_scpy_monitor_table = {}
       |_scpy_condition_table = {}
       |
       |def _scpy_lookup_runtime_state(obj, attr_name, table):
       |    value = getattr(obj, attr_name, None)
       |    if value is not None:
       |        return value
       |    return table.get(id(obj))
       |
       |def _scpy_attach_runtime_state(obj, attr_name, table, value):
       |    try:
       |        setattr(obj, attr_name, value)
       |    except (AttributeError, TypeError):
       |        table[id(obj)] = value
       |        try:
       |            _scpy_weakref.finalize(obj, table.pop, id(obj), None)
       |        except TypeError:
       |            pass
       |    return value
       |
       |def _scpy_monitor_for(obj):
       |    monitor = _scpy_lookup_runtime_state(obj, "__scpy_monitor__", _scpy_monitor_table)
       |    if monitor is not None:
       |        return monitor
       |    with _scpy_monitor_bootstrap:
       |        monitor = _scpy_lookup_runtime_state(obj, "__scpy_monitor__", _scpy_monitor_table)
       |        if monitor is not None:
       |            return monitor
       |        return _scpy_attach_runtime_state(
       |            obj,
       |            "__scpy_monitor__",
       |            _scpy_monitor_table,
       |            _scpy_threading.RLock(),
       |        )
       |
       |def _scpy_condition_for(obj):
       |    condition = _scpy_lookup_runtime_state(obj, "__scpy_condition__", _scpy_condition_table)
       |    if condition is not None:
       |        return condition
       |    with _scpy_monitor_bootstrap:
       |        condition = _scpy_lookup_runtime_state(obj, "__scpy_condition__", _scpy_condition_table)
       |        if condition is not None:
       |            return condition
       |        return _scpy_attach_runtime_state(
       |            obj,
       |            "__scpy_condition__",
       |            _scpy_condition_table,
       |            _scpy_threading.Condition(_scpy_monitor_for(obj)),
       |        )
       |
       |def _scpy_monitor_owned(monitor):
       |    is_owned = getattr(monitor, "_is_owned", None)
       |    if is_owned is None:
       |        return False
       |    return is_owned()
       |
       |def _scpy_require_monitor(obj):
       |    monitor = _scpy_monitor_for(obj)
       |    if not _scpy_monitor_owned(monitor):
       |        # Route the throw through the Scala-side helper so the
       |        # linker keeps `IllegalMonitorStateException` reachable
       |        # wherever `ThrowablesSupport` is (i.e., everywhere via
       |        # `requireNonNull`). Raising the class by bare Python
       |        # name here would hit `NameError` in tests that don't
       |        # otherwise reference `IllegalMonitorStateException`.
       |        _scpy_mod_java_lang_ThrowablesSupport__.throwIllegalMonitorState__V()
       |    return monitor
       |
       |def _scpy_object_wait(obj, timeout_seconds=None):
       |    _scpy_require_monitor(obj)
       |    _scpy_condition_for(obj).wait(timeout_seconds)
       |    return None
       |
       |def _scpy_object_notify(obj):
       |    _scpy_require_monitor(obj)
       |    _scpy_condition_for(obj).notify(1)
       |    return None
       |
       |def _scpy_object_notify_all(obj):
       |    _scpy_require_monitor(obj)
       |    _scpy_condition_for(obj).notify_all()
       |    return None
       |
       |class _scpy_Object:
       |    def getClass__Ljava_lang_Class(self):
       |        return _scpy_class_of_instance(self)
       |
       |    def toString__Ljava_lang_String(self):
       |        return self.getClass__Ljava_lang_Class().getName__Ljava_lang_String() + "@" + _builtins.format(_scpy_identity_hash_code(self), "x")
       |
       |    def __str__(self):
       |        return self.toString__Ljava_lang_String()
       |
       |    def wait__V(self):
       |        return _scpy_object_wait(self)
       |
       |    def wait__J__V(self, millis):
       |        timeout = None if millis == 0 else millis / 1000.0
       |        return _scpy_object_wait(self, timeout)
       |
       |    def wait__J_I__V(self, millis, nanos):
       |        total_nanos = millis * 1000000 + nanos
       |        timeout = None if total_nanos == 0 else total_nanos / 1000000000.0
       |        return _scpy_object_wait(self, timeout)
       |
       |    def notify__V(self):
       |        return _scpy_object_notify(self)
       |
       |    def notifyAll__V(self):
       |        return _scpy_object_notify_all(self)
       |
       |_scpy_class_registry = {}
       |_scpy_unset = object()
       |
       |class _scpy_Class(_scpy_Object):
       |    def __init__(self, name, kind, component_type=None, py_type=None):
       |        self._scpy_name = name
       |        self._scpy_kind = kind
       |        self._scpy_component_type = component_type
       |        self._scpy_py_type = py_type
       |        self._scpy_superclass_name = None
       |        self._scpy_interface_names = ()
       |
       |    def getName__Ljava_lang_String(self):
       |        return self._scpy_name
       |
       |    def getSuperclass__Ljava_lang_Class(self):
       |        if self._scpy_kind == "primitive" or self._scpy_kind == "interface":
       |            return None
       |        if self._scpy_kind == "array":
       |            return _scpy_class_of_name("java.lang.Object")
       |        if self._scpy_name == "java.lang.Object":
       |            return None
       |        if self._scpy_superclass_name is None:
       |            return _scpy_class_of_name("java.lang.Object")
       |        return _scpy_class_of_name(self._scpy_superclass_name)
       |
       |    def getInterfaces__ALjava_lang_Class(self):
       |        if self._scpy_kind == "array":
       |            names = ("java.lang.Cloneable", "java.io.Serializable")
       |        else:
       |            names = self._scpy_interface_names
       |        return _scpy_array_value(
       |            _scpy_class_of_name("java.lang.Class"),
       |            [_scpy_class_of_name(name) for name in names],
       |        )
       |
       |    def getComponentType__Ljava_lang_Class(self):
       |        if self._scpy_kind == "array":
       |            return self._scpy_component_type
       |        return None
       |
       |    def isPrimitive__Z(self):
       |        return self._scpy_kind == "primitive"
       |
       |    def isInterface__Z(self):
       |        return self._scpy_kind == "interface"
       |
       |    def isArray__Z(self):
       |        return self._scpy_kind == "array"
       |
       |    def isInstance__Ljava_lang_Object__Z(self, value):
       |        return _scpy_is_instance(value, self)
       |
       |    def isAssignableFrom__Ljava_lang_Class__Z(self, other):
       |        return _scpy_is_assignable(self, other)
       |
       |    def getClassLoader__Ljava_lang_ClassLoader(self):
       |        if self._scpy_kind == "primitive":
       |            return None
       |        return _scpy_system_class_loader
       |
       |    def toString__Ljava_lang_String(self):
       |        if self._scpy_kind == "primitive":
       |            return self._scpy_name
       |        if self._scpy_kind == "interface":
       |            return "interface " + self._scpy_name
       |        return "class " + self._scpy_name
       |
       |    def __str__(self):
       |        return self.toString__Ljava_lang_String()
       |
       |    def __getattr__(self, name):
       |        if name.startswith("getName"):
       |            return self.getName__Ljava_lang_String
       |        if name.startswith("getSuperclass"):
       |            return self.getSuperclass__Ljava_lang_Class
       |        if name.startswith("getInterfaces"):
       |            return self.getInterfaces__ALjava_lang_Class
       |        if name.startswith("getComponentType"):
       |            return self.getComponentType__Ljava_lang_Class
       |        if name.startswith("isPrimitive"):
       |            return self.isPrimitive__Z
       |        if name.startswith("isInterface"):
       |            return self.isInterface__Z
       |        if name.startswith("isArray"):
       |            return self.isArray__Z
       |        if name.startswith("isInstance"):
       |            return self.isInstance__Ljava_lang_Object__Z
       |        if name.startswith("isAssignableFrom"):
       |            return self.isAssignableFrom__Ljava_lang_Class__Z
       |        if name.startswith("getClassLoader"):
       |            return self.getClassLoader__Ljava_lang_ClassLoader
       |        if name.startswith("toString"):
       |            return self.toString__Ljava_lang_String
       |        raise AttributeError(name)
       |
       |Class = _scpy_Class
       |
       |class _scpy_ClassModule(_scpy_Object):
       |    # Stand-in for `java.lang.Class$` (the static-method receiver
       |    # for `Class.forName`). The Scala backend lowers
       |    # `Class.forName(name)` to a static call whose Python target is
       |    # `_scpy_mod_java_lang_Class_.forName__...(name)`. There is no
       |    # Scala companion source for `java.lang.Class`, so without this
       |    # the bundle would reference an undefined module variable.
       |    # Aliased below as `_scpy_mod_java_lang_Class_` (and the
       |    # double-underscore companion form for safety).
       |    #
       |    # Semantics: always raise `ClassNotFoundException`. The Python
       |    # runtime cannot honor reflective class loading — the registry
       |    # only knows about classes the bundle already defines, and even
       |    # for those the JDK-equivalent behavior (constructing via
       |    # `Class.forName(...).newInstance()`, `getDeclaredFields()`,
       |    # etc.) is not faithfully implemented. Mirroring Scala.js, we
       |    # report "not available" so callers like
       |    # `scala.runtime.ClassValueCompat` take their feature-detect
       |    # fallback branch (`FallbackClassValue`), which is correctly
       |    # emitted by the Python backend. Honoring forName would force
       |    # `ClassValueCompat` down its `JavaClassValue` path, where the
       |    # `computeValue` override is not currently kept reachable by
       |    # link-time DCE on a runtime-provided ancestor.
       |    @staticmethod
       |    def _scpy_resolve(name):
       |        # Defer the exception-class lookup to runtime: the prelude
       |        # is emitted before user/support classes, so
       |        # `ClassNotFoundException` is not bound when this source
       |        # string is parsed.
       |        exc_cls = globals().get("java_lang_ClassNotFoundException")
       |        if exc_cls is None:
       |            exc_cls = globals().get("ClassNotFoundException")
       |        if exc_cls is not None:
       |            # `_scpy_java_Throwable.__init__` (the synthesized one
       |            # from `library-py`) only accepts the 4-arg shape
       |            # `(message, cause, enableSuppression, writableStackTrace)`.
       |            raise exc_cls(name, None, True, True)
       |        # Last-resort fallback for tests where ClassNotFoundException
       |        # has been DCE'd out of the bundle.
       |        raise Exception("ClassNotFoundException: " + str(name))
       |
       |    def forName__Ljava_lang_String__Ljava_lang_Class(self, name):
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def forName__Ljava_lang_String_Z_Ljava_lang_ClassLoader__Ljava_lang_Class(self, name, initialize, loader):
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def forName__Ljava_lang_String_Ljava_lang_Module__Ljava_lang_Class(self, name, module):
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def __getattr__(self, name):
       |        # Tolerate any other encoded `forName` overload by
       |        # delegating to the resolver. The first positional argument
       |        # is always the class name.
       |        if name.startswith("forName"):
       |            def _resolve(*args, **kwargs):
       |                return _scpy_ClassModule._scpy_resolve(args[0])
       |            return _resolve
       |        raise AttributeError(name)
       |
       |class _scpy_Array(list):
       |    def __init__(self, values, clazz):
       |        super().__init__(values)
       |        self._scpy_class = clazz
       |
       |    def getClass__Ljava_lang_Class(self):
       |        return self._scpy_class
       |
       |    def clone__Ljava_lang_Object(self):
       |        return _scpy_Array(self, self._scpy_class)
       |
       |def _scpy_class_of_name(name, kind="class"):
       |    clazz = _scpy_class_registry.get(name)
       |    if clazz is None:
       |        clazz = _scpy_Class(name, kind)
       |        _scpy_class_registry[name] = clazz
       |    return clazz
       |
       |def _scpy_register_class(py_type, name, kind="class", superclass_name=None, interface_names=(), component_type=None):
       |    clazz = _scpy_class_registry.get(name)
       |    if clazz is None:
       |        clazz = _scpy_Class(name, kind, component_type, py_type)
       |        _scpy_class_registry[name] = clazz
       |    else:
       |        clazz._scpy_kind = kind
       |        clazz._scpy_component_type = component_type
       |        if py_type is not None:
       |            clazz._scpy_py_type = py_type
       |    clazz._scpy_superclass_name = superclass_name
       |    clazz._scpy_interface_names = tuple(interface_names)
       |    if py_type is not None:
       |        clazz._scpy_py_type = py_type
       |        try:
       |            py_type._scpy_class = clazz
       |        except (AttributeError, TypeError):
       |            pass
       |    return clazz
       |
       |def _scpy_descriptor_for_class(clazz):
       |    if clazz._scpy_kind == "primitive":
       |        return {
       |            "void": "V",
       |            "boolean": "Z",
       |            "char": "C",
       |            "byte": "B",
       |            "short": "S",
       |            "int": "I",
       |            "long": "J",
       |            "float": "F",
       |            "double": "D",
       |        }[clazz._scpy_name]
       |    if clazz._scpy_kind == "array":
       |        return clazz._scpy_name
       |    return "L" + clazz._scpy_name + ";"
       |
       |def _scpy_array_class(component_type):
       |    name = "[" + _scpy_descriptor_for_class(component_type)
       |    clazz = _scpy_class_registry.get(name)
       |    if clazz is None:
       |        clazz = _scpy_Class(name, "array", component_type, _scpy_Array)
       |        clazz._scpy_superclass_name = "java.lang.Object"
       |        clazz._scpy_interface_names = ("java.lang.Cloneable", "java.io.Serializable")
       |        _scpy_class_registry[name] = clazz
       |    return clazz
       |
       |def _scpy_default_value_for_class(component_type):
       |    if component_type._scpy_kind == "primitive":
       |        if component_type._scpy_name == "boolean":
       |            return False
       |        if component_type._scpy_name == "float" or component_type._scpy_name == "double":
       |            return 0.0
       |        if component_type._scpy_name == "void":
       |            return None
       |        return 0
       |    return None
       |
       |def _scpy_new_array(component_type, length, default=_scpy_unset):
       |    if length < 0:
       |        raise NegativeArraySizeException(length)
       |    fill = _scpy_default_value_for_class(component_type) if default is _scpy_unset else default
       |    return _scpy_Array([fill] * length, _scpy_array_class(component_type))
       |
       |def _scpy_array_value(component_type, values):
       |    return _scpy_Array(list(values), _scpy_array_class(component_type))
       |
       |def _scpy_array_clone(value):
       |    if isinstance(value, _scpy_Array):
       |        return _scpy_Array(value, value._scpy_class)
       |    return list(value)
       |
       |def _scpy_new_multi_array(component_type, dimensions):
       |    dims = list(dimensions)
       |    if len(dims) == 0:
       |        raise IllegalArgumentException("dimensions")
       |    if dims[0] < 0:
       |        raise NegativeArraySizeException(dims[0])
       |    if len(dims) == 1:
       |        return _scpy_new_array(component_type, dims[0])
       |    child_component = component_type
       |    for _ in range(len(dims) - 1):
       |        child_component = _scpy_array_class(child_component)
       |    children = [_scpy_new_multi_array(component_type, dims[1:]) for _ in range(dims[0])]
       |    return _scpy_array_value(child_component, children)
       |
       |def _scpy_class_of_instance(value):
       |    if value is None:
       |        raise NullPointerException()
       |    # `_scpy_LazyModule` is a transparent proxy — unwrap to the
       |    # actual module-class instance before reading its registered
       |    # class. Without this, `_scpy_class_of_instance(lazyMod)`
       |    # returns the LazyModule type and instanceof-checks fail.
       |    if isinstance(value, _scpy_LazyModule):
       |        value = value._scpy_ensure()
       |    if isinstance(value, _scpy_Array):
       |        return value._scpy_class
       |    py_cls = getattr(value.__class__, "_scpy_class", None)
       |    if py_cls is not None:
       |        return py_cls
       |    if isinstance(value, bool):
       |        return _scpy_class_of_name("java.lang.Boolean")
       |    if isinstance(value, int):
       |        return _scpy_class_of_name("java.lang.Integer")
       |    if isinstance(value, float):
       |        return _scpy_class_of_name("java.lang.Double")
       |    if isinstance(value, str):
       |        return _scpy_class_of_name("java.lang.String")
       |    return _scpy_class_of_name("java.lang.Object")
       |
       |def _scpy_is_assignable(target, source):
       |    if target is None or source is None:
       |        return False
       |    if target is source:
       |        return True
       |    if target._scpy_kind == "primitive" or source._scpy_kind == "primitive":
       |        return False
       |    if source._scpy_kind == "array":
       |        if target._scpy_kind == "array":
       |            if target._scpy_component_type is None or source._scpy_component_type is None:
       |                return False
       |            if (
       |                target._scpy_component_type._scpy_kind == "primitive"
       |                or source._scpy_component_type._scpy_kind == "primitive"
       |            ):
       |                return target._scpy_component_type is source._scpy_component_type
       |            return _scpy_is_assignable(target._scpy_component_type, source._scpy_component_type)
       |        return target._scpy_name in ("java.lang.Object", "java.lang.Cloneable", "java.io.Serializable")
       |
       |    seen = set()
       |
       |    def walk(clazz):
       |        if clazz is None or clazz._scpy_name in seen:
       |            return False
       |        if clazz is target:
       |            return True
       |        seen.add(clazz._scpy_name)
       |        if clazz._scpy_superclass_name is not None and walk(_scpy_class_of_name(clazz._scpy_superclass_name)):
       |            return True
       |        for iface in clazz._scpy_interface_names:
       |            if walk(_scpy_class_of_name(iface)):
       |                return True
       |        return False
       |
       |    return walk(source)
       |
       |def _scpy_is_instance(value, clazz):
       |    if value is None or clazz is None or clazz._scpy_kind == "primitive":
       |        return False
       |    if clazz._scpy_kind == "array":
       |        return isinstance(value, _scpy_Array) and _scpy_is_assignable(clazz, value._scpy_class)
       |    return _scpy_is_assignable(clazz, _scpy_class_of_instance(value))
       |
       |def _scpy_is_value_of_type(value, clazz):
       |    if clazz is None:
       |        return False
       |    if clazz._scpy_kind == "primitive":
       |        if clazz._scpy_name == "void":
       |            return value is None
       |        if clazz._scpy_name == "boolean":
       |            return isinstance(value, bool)
       |        if clazz._scpy_name in ("char", "byte", "short", "int", "long"):
       |            return isinstance(value, int) and not isinstance(value, bool)
       |        if clazz._scpy_name in ("float", "double"):
       |            return isinstance(value, float)
       |        return False
       |    return _scpy_is_instance(value, clazz)
       |
       |_scpy_primitive_void = _scpy_register_class(None, "void", "primitive")
       |_scpy_primitive_boolean = _scpy_register_class(None, "boolean", "primitive")
       |_scpy_primitive_char = _scpy_register_class(None, "char", "primitive")
       |_scpy_primitive_byte = _scpy_register_class(None, "byte", "primitive")
       |_scpy_primitive_short = _scpy_register_class(None, "short", "primitive")
       |_scpy_primitive_int = _scpy_register_class(None, "int", "primitive")
       |_scpy_primitive_long = _scpy_register_class(None, "long", "primitive")
       |_scpy_primitive_float = _scpy_register_class(None, "float", "primitive")
       |_scpy_primitive_double = _scpy_register_class(None, "double", "primitive")
       |
       |class Annotation(_scpy_Object):
       |    pass
       |
       |class StaticAnnotation(Annotation):
       |    pass
       |
       |class Comparable(_scpy_Object):
       |    pass
       |
       |class Serializable(_scpy_Object):
       |    pass
       |
       |# Scala `FunctionN` interfaces. Stdlib classes like `Set`, `Map`, etc.
       |# extend `Function1` (e.g. `trait Set[A] extends ... with (A => Boolean)`)
       |# — so Python must have a base class to inherit from at class-definition
       |# time. Runtime dispatch goes through `_scpy_Fn` for closures; these
       |# declarations are nominal bases only.
       |#
       |# `__getattr__` provides default forwarders for the Scala 2 specialized
       |# `apply_mc..._sp__...` method names that Scala 3's `SpecializeFunctions`
       |# rewrites call sites to. On the JVM these are interface default methods
       |# that box arguments and call the unspecialized `apply`, then unbox the
       |# result. In Python ints / floats / bools are unboxed values, so the
       |# fallback simply forwards to whichever non-specialized `apply__...`
       |# method the subclass exposes.
       |def _scpy_fn_specialized_forward(self, name):
       |    if not name.startswith("apply_mc") or "_sp__" not in name:
       |        raise AttributeError(name)
       |    target = None
       |    for cls in type(self).__mro__:
       |        for attr_name, attr_val in vars(cls).items():
       |            if attr_name == name:
       |                continue
       |            if not attr_name.startswith("apply__"):
       |                continue
       |            if attr_name.startswith("apply_mc"):
       |                continue
       |            if callable(attr_val):
       |                target = attr_val
       |                break
       |        if target is not None:
       |            break
       |    if target is None:
       |        raise AttributeError(name)
       |    return target.__get__(self, type(self))
       |
       |class Function0(_scpy_Object):
       |    def __getattr__(self, name):
       |        return _scpy_fn_specialized_forward(self, name)
       |
       |class Function1(_scpy_Object):
       |    def __getattr__(self, name):
       |        return _scpy_fn_specialized_forward(self, name)
       |
       |class Function2(_scpy_Object):
       |    def __getattr__(self, name):
       |        return _scpy_fn_specialized_forward(self, name)
       |
       |# Linker-only nominal stubs. Stdlib references them by name (some as
       |# bases — Stepper/Spliterator path), so Python must have a class to
       |# inherit from. Empty bodies — runtime never executes their methods.
       |class VarHandle(_scpy_Object): pass
       |class MethodHandles(_scpy_Object): pass
       |class MethodHandles_Lookup(_scpy_Object): pass
       |class ObjectInputStream(_scpy_Object): pass
       |class ObjectOutputStream(_scpy_Object): pass
       |class AbstractStringBuilder(_scpy_Object): pass
       |class AccessibleObject(_scpy_Object): pass
       |class Method(AccessibleObject): pass
       |class Field(AccessibleObject): pass
       |class Spliterator(_scpy_Object): pass
       |class Reference(_scpy_Object): pass
       |class WeakReference(Reference): pass
       |class ScalaNumber(_scpy_Object): pass
       |class PrimitiveIterator(_scpy_Object): pass
       |class PrimitiveIterator_OfInt(PrimitiveIterator): pass
       |class PrimitiveIterator_OfLong(PrimitiveIterator): pass
       |class PrimitiveIterator_OfDouble(PrimitiveIterator): pass
       |
       |class Mirror(_scpy_Object):
       |    pass
       |
       |class Mirror_Product(Mirror):
       |    def fromProduct__Lscala_Product__O(self, product):
       |        return None
       |
       |class Mirror_Sum(Mirror):
       |    def ordinal__O__I(self, value):
       |        return 0
       |
       |class Mirror_Singleton(Mirror_Product):
       |    def fromProduct__Lscala_Product__Lscala_deriving_Mirror_Singleton(self, product):
       |        return self
       |
       |class Mirror_SingletonProxy(Mirror_Product):
       |    def __init__(self, value):
       |        self.value = value
       |
       |    def fromProduct__Lscala_Product__O(self, product):
       |        return self.value
       |
       |class Enum(Comparable, Serializable):
       |    def __init__(self, name, ordinal):
       |        self._scpy_enum_name = name
       |        self._scpy_enum_ordinal = ordinal
       |
       |    def name__Ljava_lang_String(self):
       |        return self._scpy_enum_name
       |
       |    def ordinal__I(self):
       |        return self._scpy_enum_ordinal
       |
       |    def toString__Ljava_lang_String(self):
       |        return self._scpy_enum_name
       |
       |    def compareTo__Ljava_lang_Enum__I(self, other):
       |        return self._scpy_enum_ordinal - other.ordinal__I()
       |
       |    def compareTo__Ljava_lang_Object__I(self, other):
       |        return self.compareTo__Ljava_lang_Enum__I(other)
       |
       |    def clone__Ljava_lang_Object(self):
       |        raise CloneNotSupportedException("Enums are not cloneable", None)
       |
       |    def finalize__V(self):
       |        return None
       |
       |    def __str__(self):
       |        return self._scpy_enum_name
       |
       |class ClassLoader(_scpy_Object):
       |    def __init__(self, parent=None):
       |        self._scpy_parent = parent
       |
       |    def getParent__Ljava_lang_ClassLoader(self):
       |        return self._scpy_parent
       |
       |class ClassValue(_scpy_Object):
       |    def __init__(self):
       |        self._scpy_values = {}
       |
       |    def computeValue__Ljava_lang_Class__Ljava_lang_Object(self, clazz):
       |        return None
       |
       |    def get__Ljava_lang_Class__Ljava_lang_Object(self, clazz):
       |        if clazz is None:
       |            raise NullPointerException()
       |        if clazz not in self._scpy_values:
       |            self._scpy_values[clazz] = self.computeValue__Ljava_lang_Class__Ljava_lang_Object(clazz)
       |        return self._scpy_values[clazz]
       |
       |    def remove__Ljava_lang_Class__V(self, clazz):
       |        if clazz is None:
       |            raise NullPointerException()
       |        self._scpy_values.pop(clazz, None)
       |        return None
       |
       |class BoxedUnit(_scpy_Object):
       |    def __eq__(self, other):
       |        return self is other
       |
       |    def __hash__(self):
       |        return 0
       |
       |    def __str__(self):
       |        return "()"
       |
       |# Stdlib accesses `BoxedUnit.UNIT` as a static field via
       |# `LoadModule(BoxedUnit) + Select(UNIT)`. Bind the static field
       |# on the class itself, then expose the same instance under the
       |# emitter's `_scpy_mod_*_` naming convention so all callers
       |# (LoadModule, ApplyStatic) hit it.
       |BoxedUnit.UNIT = BoxedUnit()
       |BoxedUnit.TYPE = _scpy_primitive_void
       |_scpy_mod_scala_runtime_BoxedUnit_ = BoxedUnit
       |_scpy_mod_scala_runtime_BoxedUnit__ = BoxedUnit
       |
       |class _scpy_IntModule(_scpy_Object):
       |    def toChar__I__C(self, value):
       |        return chr(value & 0xFFFF)
       |
       |    def int2long__I__J(self, value):
       |        return value
       |
       |class _scpy_CharModule(_scpy_Object):
       |    def toInt__C__I(self, value):
       |        return ord(value)
       |
       |    def char2int__C__I(self, value):
       |        return ord(value)
       |
       |_scpy_mod_scala_Int_ = _scpy_IntModule()
       |_scpy_mod_scala_Char_ = _scpy_CharModule()
       |_scpy_mod_scala_Int__ = _scpy_mod_scala_Int_
       |_scpy_mod_scala_Char__ = _scpy_mod_scala_Char_
       |# `Class.forName(...)` is a static call on the JDK-provided
       |# `java.lang.Class`. The backend lowers it to
       |# `_scpy_mod_java_lang_Class_.forName__...(...)`. Bind the module
       |# variable to a singleton of `_scpy_ClassModule` so the call
       |# resolves; cover the `Class$` companion form too in case any
       |# emitted code carries the trailing-dollar variant.
       |_scpy_mod_java_lang_Class_  = _scpy_ClassModule()
       |_scpy_mod_java_lang_Class__ = _scpy_mod_java_lang_Class_
       |_scpy_system_class_loader = ClassLoader()
       |
       |_scpy_register_class(object, "java.lang.Object", "class", None)
       |_scpy_register_class(str, "java.lang.String", "class", "java.lang.Object", ("java.lang.CharSequence", "java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(_scpy_Class, "java.lang.Class", "class", "java.lang.Object")
       |_scpy_register_class(ClassLoader, "java.lang.ClassLoader", "class", "java.lang.Object")
       |_scpy_register_class(ClassValue, "java.lang.ClassValue", "class", "java.lang.Object")
       |_scpy_register_class(Function0, "scala.Function0", "interface", None)
       |_scpy_register_class(Function1, "scala.Function1", "interface", None)
       |_scpy_register_class(Function2, "scala.Function2", "interface", None)
       |_scpy_register_class(VarHandle, "java.lang.invoke.VarHandle", "class", "java.lang.Object")
       |_scpy_register_class(MethodHandles, "java.lang.invoke.MethodHandles", "class", "java.lang.Object")
       |_scpy_register_class(MethodHandles_Lookup, "java.lang.invoke.MethodHandles_Lookup", "class", "java.lang.Object")
       |_scpy_register_class(ObjectInputStream, "java.io.ObjectInputStream", "class", "java.lang.Object")
       |_scpy_register_class(ObjectOutputStream, "java.io.ObjectOutputStream", "class", "java.lang.Object")
       |_scpy_register_class(AbstractStringBuilder, "java.lang.AbstractStringBuilder", "class", "java.lang.Object")
       |_scpy_register_class(AccessibleObject, "java.lang.reflect.AccessibleObject", "class", "java.lang.Object")
       |_scpy_register_class(Method, "java.lang.reflect.Method", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Field, "java.lang.reflect.Field", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Spliterator, "java.util.Spliterator", "interface", None)
       |_scpy_register_class(Reference, "java.lang.ref.Reference", "class", "java.lang.Object")
       |_scpy_register_class(WeakReference, "java.lang.ref.WeakReference", "class", "java.lang.ref.Reference")
       |_scpy_register_class(ScalaNumber, "scala.math.ScalaNumber", "class", "java.lang.Object")
       |_scpy_register_class(PrimitiveIterator, "java.util.PrimitiveIterator", "interface", None)
       |_scpy_register_class(PrimitiveIterator_OfInt, "java.util.PrimitiveIterator_OfInt", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(PrimitiveIterator_OfLong, "java.util.PrimitiveIterator_OfLong", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(PrimitiveIterator_OfDouble, "java.util.PrimitiveIterator_OfDouble", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(Annotation, "scala.annotation.Annotation", "class", "java.lang.Object")
       |_scpy_register_class(StaticAnnotation, "scala.annotation.StaticAnnotation", "class", "scala.annotation.Annotation")
       |_scpy_register_class(Comparable, "java.lang.Comparable", "interface", None)
       |_scpy_register_class(Serializable, "java.io.Serializable", "interface", None)
       |_scpy_register_class(Mirror, "scala.deriving.Mirror", "interface", None)
       |_scpy_register_class(Mirror_Product, "scala.deriving.Mirror_Product", "interface", None, ("scala.deriving.Mirror",))
       |_scpy_register_class(Mirror_Sum, "scala.deriving.Mirror_Sum", "interface", None, ("scala.deriving.Mirror",))
       |_scpy_register_class(Mirror_Singleton, "scala.deriving.Mirror_Singleton", "interface", None, ("scala.deriving.Mirror_Product",))
       |_scpy_register_class(Mirror_SingletonProxy, "scala.deriving.Mirror_SingletonProxy", "class", "java.lang.Object", ("scala.deriving.Mirror_Product",))
       |_scpy_register_class(Enum, "java.lang.Enum", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(BoxedUnit, "scala.runtime.BoxedUnit", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.Int_", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.Char_", "class", "java.lang.Object")
       |_scpy_register_class(None, "java.lang.Cloneable", "interface", None)
       |_scpy_register_class(None, "java.lang.Number", "class", "java.lang.Object", ("java.io.Serializable",))
       |_scpy_register_class(None, "java.lang.Boolean", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Character", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Byte", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Short", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Integer", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Long", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Float", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Double", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |
       |# -- lazy module init --
       |# Wraps a module class so its `__init__` runs at most once on
       |# first attribute access. Breaks Python's strict left-to-right
       |# class-init order: modules with cross-references (Console →
       |# System.out, MapNode → ClassTag.Any, …) only initialize when
       |# user code actually accesses them, not when their class is
       |# defined.
       |#
       |# Implementation: a transparent proxy. `__getattr__` triggers
       |# init then delegates. Direct attribute writes go straight to
       |# the underlying instance so `__init__` body works normally
       |# (it sets `self.x = ...` which goes through the proxy's
       |# `__setattr__`).
       |class _scpy_LazyModule:
       |    __slots__ = ("_scpy_cls", "_scpy_inst", "_scpy_init_started")
       |    def __init__(self, cls):
       |        object.__setattr__(self, "_scpy_cls", cls)
       |        object.__setattr__(self, "_scpy_inst", cls.__new__(cls))
       |        object.__setattr__(self, "_scpy_init_started", False)
       |    def _scpy_ensure(self):
       |        if not object.__getattribute__(self, "_scpy_init_started"):
       |            object.__setattr__(self, "_scpy_init_started", True)
       |            inst = object.__getattribute__(self, "_scpy_inst")
       |            inst.__init__()
       |        return object.__getattribute__(self, "_scpy_inst")
       |    def __getattr__(self, name):
       |        # __getattr__ runs only when normal lookup fails — so the
       |        # __slots__ above are reached via __getattribute__ and
       |        # don't recurse here.
       |        inst = self._scpy_ensure()
       |        return getattr(inst, name)
       |    def __setattr__(self, name, value):
       |        inst = object.__getattribute__(self, "_scpy_inst")
       |        setattr(inst, name, value)
       |    def __call__(self, *args, **kw):
       |        return self._scpy_ensure()(*args, **kw)
       |    def __repr__(self):
       |        return f"<LazyModule {object.__getattribute__(self, '_scpy_cls').__name__}>"
       |
       |def _scpy_module_value(module):
       |    ensure = getattr(module, "_scpy_ensure", None)
       |    if ensure is None:
       |        return module
       |    return ensure()
       |
       |def _scpy_lazy_module(cls):
       |    return _scpy_LazyModule(cls)
       |
       |class _scpy_Fn(_scpy_Object):
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
       |    class _Ref(_scpy_Object):
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
       |_scpy_register_class(IntRef, "scala.runtime.IntRef", "class", "java.lang.Object")
       |_scpy_register_class(LongRef, "scala.runtime.LongRef", "class", "java.lang.Object")
       |_scpy_register_class(DoubleRef, "scala.runtime.DoubleRef", "class", "java.lang.Object")
       |_scpy_register_class(FloatRef, "scala.runtime.FloatRef", "class", "java.lang.Object")
       |_scpy_register_class(BooleanRef, "scala.runtime.BooleanRef", "class", "java.lang.Object")
       |_scpy_register_class(ByteRef, "scala.runtime.ByteRef", "class", "java.lang.Object")
       |_scpy_register_class(CharRef, "scala.runtime.CharRef", "class", "java.lang.Object")
       |_scpy_register_class(ShortRef, "scala.runtime.ShortRef", "class", "java.lang.Object")
       |_scpy_register_class(ObjectRef, "scala.runtime.ObjectRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileIntRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileLongRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileDoubleRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileFloatRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileBooleanRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileByteRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileCharRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileShortRef", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.VolatileObjectRef", "class", "java.lang.Object")
       |
       |""".stripMargin +
    """|# -- Compiler-invented: numeric wrapping (Scala overflow semantics) --
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
       |def _scpy_float_to_str(x):
       |    if _scpy_math.isnan(x):
       |        return "NaN"
       |    if _scpy_math.isinf(x):
       |        return "Infinity" if x > 0 else "-Infinity"
       |    return _builtins.str(x)
       |
       |def _scpy_to_str(x):
       |    # Scala-faithful stringification: matches `String.valueOf`
       |    if x is None:
       |        return "null"
       |    if x is True:
       |        return "true"
       |    if x is False:
       |        return "false"
       |    to_string = getattr(x, "toString__Ljava_lang_String", None)
       |    if to_string is not None:
       |        return to_string()
       |    return _builtins.str(x)
       |
       |def _scpy_call_to_string(x):
       |    if x is None:
       |        raise NullPointerException()
       |    return _scpy_to_str(x)
       |
       |def _scpy_int_to_string_radix(value, radix):
       |    if radix < 2 or radix > 36:
       |        radix = 10
       |    if value == 0:
       |        return "0"
       |    negative = value < 0
       |    digits = "0123456789abcdefghijklmnopqrstuvwxyz"
       |    value = -value if negative else value
       |    out = []
       |    while value:
       |        value, rem = divmod(value, radix)
       |        out.append(digits[rem])
       |    if negative:
       |        out.append("-")
       |    out.reverse()
       |    return "".join(out)
       |
       |def _scpy_int_to_signed_bytes(value):
       |    if value == 0:
       |        length = 1
       |    elif value > 0:
       |        length = (value.bit_length() + 8) // 8
       |    else:
       |        length = ((~value).bit_length() + 8) // 8
       |    return value.to_bytes(length, "big", signed=True)
       |
       |def _scpy_int_from_signed_bytes(data):
       |    return _builtins.int.from_bytes(_builtins.bytes(data), "big", signed=True)
       |
       |def _scpy_int_from_unsigned_bytes(data):
       |    return _builtins.int.from_bytes(_builtins.bytes(data), "big", signed=False)
       |
       |def _scpy_int_trunc_div(a, b):
       |    if b == 0:
       |        raise ZeroDivisionError()
       |    quot = abs(a) // abs(b)
       |    return -quot if (a < 0) ^ (b < 0) else quot
       |
       |def _scpy_int_signum(value):
       |    if value > 0:
       |        return 1
       |    if value < 0:
       |        return -1
       |    return 0
       |
       |def _scpy_int_compare(a, b):
       |    if a < b:
       |        return -1
       |    if a > b:
       |        return 1
       |    return 0
       |
       |def _scpy_int_bit_length(value):
       |    if value < 0:
       |        value = ~value
       |    return value.bit_length()
       |
       |def _scpy_int_bit_count(value):
       |    if value < 0:
       |        value = ~value
       |    return value.bit_count()
       |
       |def _scpy_int_lowest_set_bit(value):
       |    if value == 0:
       |        return -1
       |    return (value & -value).bit_length() - 1
       |
       |def _scpy_int_test_bit(value, index):
       |    if index < 0:
       |        raise ValueError("negative bit index")
       |    return ((value >> index) & 1) != 0
       |
       |def _scpy_int_to_double(value):
       |    try:
       |        return _builtins.float(value)
       |    except OverflowError:
       |        return _builtins.float("inf") if value > 0 else _builtins.float("-inf")
       |
       |def _scpy_decimal_plain_string(value):
       |    sign, digits, exp = value.as_tuple()
       |    sign_str = "-" if sign else ""
       |    digits_str = "".join(_builtins.str(d) for d in digits) or "0"
       |    if all(d == 0 for d in digits):
       |        if exp >= 0:
       |            return sign_str + "0"
       |        return sign_str + "0." + ("0" * (-exp))
       |    if exp >= 0:
       |        return sign_str + digits_str + ("0" * exp)
       |    point = len(digits) + exp
       |    if point <= 0:
       |        return sign_str + "0." + ("0" * (-point)) + digits_str
       |    return sign_str + digits_str[:point] + "." + digits_str[point:]
       |
       |def _scpy_decimal_with_context(precision, rounding, thunk):
       |    if precision <= 0:
       |        try:
       |            return thunk()
       |        except _scpy_decimal.DecimalException as err:
       |            raise ArithmeticException(_builtins.str(err))
       |    ctx = _scpy_decimal.getcontext().copy()
       |    ctx.prec = precision
       |    if rounding == "ROUND_UNNECESSARY":
       |        ctx.rounding = _scpy_decimal.ROUND_HALF_UP
       |        ctx.traps[_scpy_decimal.Inexact] = True
       |        ctx.traps[_scpy_decimal.Rounded] = True
       |    else:
       |        ctx.rounding = rounding
       |    with _scpy_decimal.localcontext(ctx):
       |        try:
       |            return thunk()
       |        except _scpy_decimal.DecimalException as err:
       |            raise ArithmeticException(_builtins.str(err))
       |
       |def _scpy_decimal_unary_with_context(op, value, precision, rounding):
       |    ctx = _scpy_decimal.getcontext().copy()
       |    ctx.prec = 1 if precision <= 0 else precision
       |    if rounding == "ROUND_UNNECESSARY":
       |        ctx.rounding = _scpy_decimal.ROUND_HALF_UP
       |        ctx.traps[_scpy_decimal.Inexact] = True
       |        ctx.traps[_scpy_decimal.Rounded] = True
       |    else:
       |        ctx.rounding = rounding
       |    with _scpy_decimal.localcontext(ctx):
       |        try:
       |            if op == "plus":
       |                return +value
       |            if op == "negate":
       |                return -value
       |            raise ValueError(f"unsupported decimal unary op: {op}")
       |        except _scpy_decimal.DecimalException as err:
       |            raise ArithmeticException(_builtins.str(err))
       |
       |def _scpy_decimal_binary_with_context(op, lhs, rhs, precision, rounding):
       |    ctx = _scpy_decimal.getcontext().copy()
       |    ctx.prec = 1 if precision <= 0 else precision
       |    if rounding == "ROUND_UNNECESSARY":
       |        ctx.rounding = _scpy_decimal.ROUND_HALF_UP
       |        ctx.traps[_scpy_decimal.Inexact] = True
       |        ctx.traps[_scpy_decimal.Rounded] = True
       |    else:
       |        ctx.rounding = rounding
       |    with _scpy_decimal.localcontext(ctx):
       |        try:
       |            if op == "add":
       |                return lhs + rhs
       |            if op == "subtract":
       |                return lhs - rhs
       |            if op == "multiply":
       |                return lhs * rhs
       |            if op == "divide":
       |                return lhs / rhs
       |            if op == "divideToIntegral":
       |                return lhs // rhs
       |            if op == "remainder":
       |                return lhs % rhs
       |            if op == "pow":
       |                return lhs ** rhs
       |            raise ValueError(f"unsupported decimal binary op: {op}")
       |        except _scpy_decimal.DecimalException as err:
       |            raise ArithmeticException(_builtins.str(err))
       |
       |def _scpy_decimal_unscaled_and_scale(value):
       |    sign, digits, exp = value.as_tuple()
       |    coeff = 0
       |    for digit in digits:
       |        coeff = coeff * 10 + digit
       |    if sign:
       |        coeff = -coeff
       |    return (coeff, -exp)
       |
       |def _scpy_decimal_precision(value):
       |    return len(value.as_tuple().digits)
       |
       |def _scpy_decimal_quantize(value, scale, rounding):
       |    exp = _scpy_decimal.Decimal(1).scaleb(-scale)
       |    ctx = _scpy_decimal.getcontext().copy()
       |    if rounding == "ROUND_UNNECESSARY":
       |        ctx.rounding = _scpy_decimal.ROUND_HALF_UP
       |        ctx.traps[_scpy_decimal.Inexact] = True
       |        ctx.traps[_scpy_decimal.Rounded] = True
       |    else:
       |        ctx.rounding = rounding
       |    with _scpy_decimal.localcontext(ctx):
       |        try:
       |            return value.quantize(exp)
       |        except _scpy_decimal.DecimalException as err:
       |            raise ArithmeticException(_builtins.str(err))
       |
       |def _scpy_decimal_to_pyint(value):
       |    return _builtins.int(value)
       |
       |def _scpy_decimal_compare(lhs, rhs):
       |    if lhs < rhs:
       |        return -1
       |    if lhs > rhs:
       |        return 1
       |    return 0
       |
       |def _scpy_decimal_signum(value):
       |    if value.is_zero():
       |        return 0
       |    return -1 if value < 0 else 1
       |
       |def _scpy_decimal_to_double(value):
       |    return _builtins.float(value)
       |
       |def _scpy_identity_hash_code(obj):
       |    if obj is None:
       |        return 0
       |    # Python can reuse ids after object death; within an object's
       |    # lifetime this matches the stable identity hash we need.
       |    return id(obj) & 0x7FFFFFFF
       |
       |def _scpy_arraycopy(src, src_pos, dst, dst_pos, length):
       |    if src is None or dst is None:
       |        raise NullPointerException()
       |    if src_pos < 0 or dst_pos < 0 or length < 0:
       |        raise ArrayIndexOutOfBoundsException(length)
       |    if src_pos + length > len(src):
       |        raise ArrayIndexOutOfBoundsException(src_pos + length)
       |    if dst_pos + length > len(dst):
       |        raise ArrayIndexOutOfBoundsException(dst_pos + length)
       |    dst[dst_pos:dst_pos + length] = src[src_pos:src_pos + length]
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
       |    # Java's Character.isWhitespace excludes these three non-breaking
       |    # spaces; Python's str.isspace() includes them.
       |    if ch == '\u00A0' or ch == '\u2007' or ch == '\u202F':
       |        return False
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
       |    out = _scpy_new_array(_scpy_primitive_char, len(s), 0)
       |    for i, ch in enumerate(s):
       |        out[i] = ord(ch)
       |    return out
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
       |def _scpy_codec_lookup(name):
       |    import encodings
       |    info = encodings.search_function(name)
       |    if info is None:
       |        return None
       |    return info.name
       |
       |def _scpy_codec_decode_step(decoder, data, final_chunk):
       |    saved_state = decoder.getstate()
       |    try:
       |        text = decoder.decode(data, final_chunk)
       |        state = decoder.getstate()
       |        buffered = 0
       |        if state is not None:
       |            buffered_input = state[0]
       |            if buffered_input is not None:
       |                buffered = len(buffered_input)
       |        consumed = len(data) - buffered
       |        if buffered > 0:
       |            decoder.setstate(saved_state)
       |            prefix = decoder.decode(data[:consumed], False) if consumed > 0 else ''
       |            return (prefix, consumed, -1, -1, None)
       |        return (text, len(data), -1, -1, None)
       |    except UnicodeError as err:
       |        decoder.setstate(saved_state)
       |        prefix = decoder.decode(data[:err.start], False) if err.start > 0 else ''
       |        return (prefix, err.start, err.start, err.end, getattr(err, 'reason', None))
       |
       |def _scpy_codec_encode_step(encoder, text, final_chunk):
       |    saved_state = encoder.getstate()
       |    try:
       |        data = encoder.encode(text, final_chunk)
       |        return (data, len(text), -1, -1, None)
       |    except UnicodeError as err:
       |        encoder.setstate(saved_state)
       |        prefix = encoder.encode(text[:err.start], False) if err.start > 0 else b''
       |        return (prefix, err.start, err.start, err.end, getattr(err, 'reason', None))
       |
       |def _scpy_str_get_bytes(s, *encoding):
       |    # When called with a Charset instance, route through the
       |    # Charset.encode(String) method so every charset-specific
       |    # quirk (e.g. UTF-16's big-endian BOM, custom replacement
       |    # bytes) matches the Java-layer behaviour. The string-named
       |    # and no-arg overloads stay on the direct `str.encode`
       |    # fast path.
       |    if encoding and encoding[0] is not None and (
       |            hasattr(encoding[0], 'encode__Ljava_lang_String__Ljava_nio_ByteBuffer')
       |    ):
       |        bb = encoding[0].encode__Ljava_lang_String__Ljava_nio_ByteBuffer(s)
       |        remaining = bb.remaining__I()
       |        out = _scpy_new_array(_scpy_primitive_byte, remaining, 0)
       |        if remaining > 0:
       |            bb.get__AB__Ljava_nio_ByteBuffer(out)
       |        return out
       |    errors = 'strict'
       |    if not encoding:
       |        enc = 'UTF-8'
       |    else:
       |        candidate = encoding[0]
       |        if isinstance(candidate, str):
       |            enc = candidate
       |        elif candidate is None:
       |            raise NullPointerException()
       |        elif hasattr(candidate, 'name__Ljava_lang_String'):
       |            enc = candidate.name__Ljava_lang_String()
       |            errors = 'replace'
       |        elif hasattr(candidate, 'name'):
       |            enc = candidate.name()
       |            errors = 'replace'
       |        else:
       |            raise UnsupportedCharsetException(_builtins.str(candidate))
       |    try:
       |        raw = s.encode(enc, errors)
       |    except LookupError:
       |        raise UnsupportedCharsetException(enc)
       |    out = _scpy_new_array(_scpy_primitive_byte, len(raw), 0)
       |    for i, b in enumerate(raw):
       |        out[i] = b - 256 if b >= 128 else b
       |    return out
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
       |    if min_leading is None:
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
