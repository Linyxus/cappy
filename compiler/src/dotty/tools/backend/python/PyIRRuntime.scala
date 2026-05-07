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
  private val ComparableClass = PyClassName("java.lang.Comparable")
  private val EnumClass = PyClassName("java.lang.Enum")
  private val ClassLoaderClass = PyClassName("java.lang.ClassLoader")
  private val ClassValueClass = PyClassName("java.lang.ClassValue")

  // Scala's by-ref closure-capture wrappers ship as `.pyir` from
  // `library-py/src/scala/runtime/` (Category A migration of
  // `notes/shrink-runtime.md`). BoxedUnit remains runtime-provided —
  // the Scala port is blocked on `defn.BoxedUnit_UNIT`'s linkedClass
  // resolver / @static interaction with the Python emitter's module
  // access shape; deferred until the broader BoxedUnit_UNIT lookup
  // path is taught to recognise @static-lifted module members.
  private val BoxedUnitClass = PyClassName("scala.runtime.BoxedUnit")

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
  private val AbstractStringBuilderClass = PyClassName("java.lang.AbstractStringBuilder")
  private val ReflectMethodClass         = PyClassName("java.lang.reflect.Method")
  private val ReflectFieldClass          = PyClassName("java.lang.reflect.Field")
  private val ReflectConstructorClass    = PyClassName("java.lang.reflect.Constructor")
  private val ReflectAccessibleObjClass  = PyClassName("java.lang.reflect.AccessibleObject")
  private val ReflectExecutableClass     = PyClassName("java.lang.reflect.Executable")
  private val ReflectModifierClass       = PyClassName("java.lang.reflect.Modifier")
  private val ReflectTypeClass           = PyClassName("java.lang.reflect.Type")
  private val ReflectTypeVariableClass   = PyClassName("java.lang.reflect.TypeVariable")
  private val ReflectInvocationTargetExceptionClass =
    PyClassName("java.lang.reflect.InvocationTargetException")
  private val SpliteratorClass           = PyClassName("java.util.Spliterator")
  private val RefReferenceClass          = PyClassName("java.lang.ref.Reference")
  private val RefWeakReferenceClass      = PyClassName("java.lang.ref.WeakReference")
  // `scala.math.ScalaNumber` was previously a runtime-provided empty
  // stub. Phase 4 of Category A migration moves it to
  // `library-py/src/scala/math/ScalaNumber.scala` so the abstract
  // `isWhole` / `underlying` methods actually exist on the class.
  private val PrimitiveIteratorClass     = PyClassName("java.util.PrimitiveIterator")
  private val PrimitiveIteratorOfIntClass    = PyClassName("java.util.PrimitiveIterator_OfInt")
  private val PrimitiveIteratorOfLongClass   = PyClassName("java.util.PrimitiveIterator_OfLong")
  private val PrimitiveIteratorOfDoubleClass = PyClassName("java.util.PrimitiveIterator_OfDouble")

  // Scala tuple classes are runtime-provided when `-scalapy` is on.
  // GenPython lowers tuple values to native Python `tuple`s, so the
  // compiled `.pyir` for the Scala-side tuple classes is intentionally
  // dropped at link time. `javaProvided = true` makes any unintercepted
  // method call link cleanly and surface as an `AttributeError` at
  // runtime rather than a hard link error.
  private val ScalaTupleClass            = PyClassName("scala.Tuple")
  private val ScalaNonEmptyTupleClass    = PyClassName("scala.NonEmptyTuple")
  private val ScalaPairClass             = PyClassName("scala._times_colon")
  private val ScalaEmptyTupleModuleClass = PyClassName("scala.EmptyTuple_")
  private val ScalaTupleXXLClass         = PyClassName("scala.runtime.TupleXXL")
  private val ScalaRuntimeTuplesClass    = PyClassName("scala.runtime.Tuples_")
  private val ScalaTupleNClasses: List[PyClassName] =
    (1 to 22).map(n => PyClassName(s"scala.Tuple$n")).toList

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
        // Accept any reflective-API method or stub. Callers depend on
        // a wide surface (`getDeclaredFields`, `getEnclosingMethod`,
        // `getEnumConstants`, `getModifiers`, `getAnnotation*`, ...);
        // the runtime stubs in `_scpy_Class` cover them and dispatch
        // through `__getattr__` for any missed signature.
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set("forName"))
      ),
    ClassLoaderClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(
          simpleNamePrefixes = Set("getParent", "loadClass", "findClass", "getResource", "getResourceAsStream", "getResources")
        )
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
    ReflectExecutableClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ReflectAccessibleObjClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectConstructorClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(ReflectAccessibleObjClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectModifierClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectTypeClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectTypeVariableClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(ReflectTypeClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ReflectInvocationTargetExceptionClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
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
    ScalaTupleClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaNonEmptyTupleClass ->
      ProvidedClass(
        kind = PyClassKind.Interface,
        superClass = None,
        interfaces = List(ScalaTupleClass),
        javaProvided = true,
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaPairClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        interfaces = List(ScalaNonEmptyTupleClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaEmptyTupleModuleClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        interfaces = List(ScalaTupleClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaTupleXXLClass ->
      ProvidedClass(
        kind = PyClassKind.Class,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
    ScalaRuntimeTuplesClass ->
      ProvidedClass(
        kind = PyClassKind.ModuleClass,
        superClass = Some(PyClassName.ObjectClass),
        javaProvided = true,
        constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
        instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
        staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
      ),
  ) ++ ScalaTupleNClasses.map { cn =>
    cn -> ProvidedClass(
      kind = PyClassKind.Class,
      superClass = Some(PyClassName.ObjectClass),
      interfaces = List(ScalaPairClass, ScalaNonEmptyTupleClass),
      javaProvided = true,
      constructors = MethodMatcher(simpleNamePrefixes = Set("<init>")),
      instanceMethods = MethodMatcher(simpleNamePrefixes = Set("")),
      staticMethods = MethodMatcher(simpleNamePrefixes = Set(""))
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
    ),
    // `_scpy_idiv` / `_scpy_imod` / `_scpy_ldiv` / `_scpy_lmod` (see
    // `prelude`) translate Python `ZeroDivisionError` into a JDK
    // `ArithmeticException` so divide-by-zero can flow into Scala
    // `Try` / typed `catch` clauses. The instantiation here keeps every
    // constructor of `ArithmeticException` live even when no Scala-side
    // call site references the class directly.
    PyClassName("java.lang.ArithmeticException") -> PyMethodName(
      PySimpleMethodName.Constructor,
      List(PyClassRef(PyClassName("java.lang.String"))),
      PyPrimRef.VoidRef
    ),
  )

  /** Virtual-call seeds for runtime-prelude helpers that dispatch on an
   *  interface type. Unlike [[preludeCalls]] (which marks the owner as
   *  instantiated and analyzes the method body), these entries log a
   *  virtual call against the static receiver so the reachability engine
   *  propagates the method to every subtype that ends up instantiated.
   *
   *  Used by `_scpy_charseq_*` (in `prelude`): codegen rewrites
   *  `cs.subSequence(...)` etc. into `_scpy_charseq_sub_sequence(cs, ...)`.
   *  The runtime helper falls back to `recv.subSequence__I_I__...(...)`
   *  for non-`str` receivers (`StringBuilder`, `ArrayCharSequence`,
   *  `SeqCharSequence`, ...). Without these seeds the analyzer never
   *  sees a virtual call on `CharSequence.subSequence`, so the subtype
   *  overrides get DCE'd and the helper's fallback `AttributeError`s.
   */
  private[python] val virtualCallSeeds: List[(PyClassName, PyMethodName)] = List(
    // `_scpy_product_*` helpers polyfill `Product` methods. Every
    // statically-visible call site to `Product.productIterator` etc.
    // is redirected by GenPython to one of these helpers, which then
    // falls back to `recv.productMethod()` for non-tuple receivers.
    // Seed the Product method symbols as virtual targets so case-class
    // / anon-class overrides stay reachable.
    PyClassName("scala.Product") -> PyMethodName(
      PySimpleMethodName("productIterator"),
      Nil,
      PyClassRef(PyClassName("scala.collection.Iterator"))
    ),
    PyClassName("scala.Product") -> PyMethodName(
      PySimpleMethodName("productArity"),
      Nil,
      PyPrimRef.IntRef
    ),
    PyClassName("scala.Product") -> PyMethodName(
      PySimpleMethodName("productPrefix"),
      Nil,
      PyClassRef(PyClassName("java.lang.String"))
    ),
    PyClassName("scala.Product") -> PyMethodName(
      PySimpleMethodName("productElement"),
      List(PyPrimRef.IntRef),
      PyClassRef(PyClassName.ObjectClass)
    ),
    PyClassName("scala.Product") -> PyMethodName(
      PySimpleMethodName("productElementName"),
      List(PyPrimRef.IntRef),
      PyClassRef(PyClassName("java.lang.String"))
    ),
    PyClassName("java.lang.CharSequence") -> PyMethodName(
      PySimpleMethodName("subSequence"),
      List(PyPrimRef.IntRef, PyPrimRef.IntRef),
      PyClassRef(PyClassName("java.lang.CharSequence"))
    ),
    PyClassName("java.lang.CharSequence") -> PyMethodName(
      PySimpleMethodName("length"),
      Nil,
      PyPrimRef.IntRef
    ),
    PyClassName("java.lang.CharSequence") -> PyMethodName(
      PySimpleMethodName("charAt"),
      List(PyPrimRef.IntRef),
      PyPrimRef.CharRef
    ),
    PyClassName("java.lang.CharSequence") -> PyMethodName(
      PySimpleMethodName("isEmpty"),
      Nil,
      PyPrimRef.BooleanRef
    ),
    PyClassName("java.lang.CharSequence") -> PyMethodName(
      PySimpleMethodName("toString"),
      Nil,
      PyClassRef(PyClassName("java.lang.String"))
    ),
    // Boxed-primitive instance methods that the `_scpy_Double_*` /
    // `_scpy_Boolean_*` helpers (in `prelude`) fall through to when the
    // runtime receiver isn't a raw Python primitive. Without these seeds
    // the analyzer never sees a virtual call on the boxed-primitive
    // method, so the pylib subtype implementations get DCE'd and the
    // helper's fallback `AttributeError`s on the rare ported-box receiver.
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("isNaN"), Nil, PyPrimRef.BooleanRef
    ),
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("isInfinite"), Nil, PyPrimRef.BooleanRef
    ),
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("doubleValue"), Nil, PyPrimRef.DoubleRef
    ),
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("floatValue"), Nil, PyPrimRef.FloatRef
    ),
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("intValue"), Nil, PyPrimRef.IntRef
    ),
    PyClassName("java.lang.Double") -> PyMethodName(
      PySimpleMethodName("longValue"), Nil, PyPrimRef.LongRef
    ),
    PyClassName("java.lang.Boolean") -> PyMethodName(
      PySimpleMethodName("booleanValue"), Nil, PyPrimRef.BooleanRef
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

  /** Scala FQCNs whose simple name collides with a Python builtin and
   *  must be remapped at emission time (see [[classIdentifier]]).
   *
   *  Only `java.lang.Exception` is structurally problematic: we emit
   *  `class Throwable(Exception):` against Python's builtin, so the
   *  Scala `class Exception(Throwable):` definition would rebind
   *  `Exception` in module scope. Keep this narrow — a user-defined
   *  Scala class named `Exception` in a different package stays free
   *  to emit `class Exception(...)` normally. Other potential
   *  collisions like `java.lang.Error` are not Python builtins, so
   *  they pass through.
   */
  val PythonReservedShortNames: Map[String, String] = Map(
    "java.lang.Exception" -> "_scpy_java_Exception",
    "java.lang.Object"    -> "_scpy_Object",
    "java.lang.String"    -> "str",
    "java.lang.Class"     -> "_scpy_Class",
    // Pylib exception classes referenced by the runtime prelude (e.g.
    // `_scpy_str_repeat` raises `IllegalArgumentException`). The prelude
    // uses the simple name because it was hand-written; aliasing here
    // makes the emitter's FQN-mangled identifier match those references
    // when the class is defined downstream in the bundle.
    "java.lang.NullPointerException"        -> "NullPointerException",
    "java.lang.IllegalArgumentException"    -> "IllegalArgumentException",
    "java.lang.IllegalStateException"       -> "IllegalStateException",
    "java.lang.IndexOutOfBoundsException"   -> "IndexOutOfBoundsException",
    "java.lang.ArrayIndexOutOfBoundsException" -> "ArrayIndexOutOfBoundsException",
    "java.lang.ArithmeticException"         -> "ArithmeticException",
    "java.lang.UnsupportedOperationException" -> "UnsupportedOperationException",
    "java.lang.ClassCastException"          -> "ClassCastException",
    "java.lang.NumberFormatException"       -> "NumberFormatException",
    "java.lang.RuntimeException"            -> "RuntimeException",
    "java.lang.Throwable"                   -> "_scpy_java_Throwable",
    "java.lang.Error"                       -> "_scpy_java_Error",
    "java.lang.AssertionError"              -> "AssertionError",
    "java.lang.OutOfMemoryError"            -> "OutOfMemoryError",
    "java.lang.StackOverflowError"          -> "StackOverflowError",
    "java.lang.NoSuchFieldException"        -> "NoSuchFieldException",
    "java.lang.NoSuchMethodException"       -> "NoSuchMethodException",
    "java.lang.CloneNotSupportedException"  -> "CloneNotSupportedException",
    "java.lang.IllegalAccessException"      -> "IllegalAccessException",
    "java.lang.InterruptedException"        -> "InterruptedException",
    "java.lang.SecurityException"           -> "SecurityException",
    "java.lang.NegativeArraySizeException"  -> "NegativeArraySizeException",
    "java.lang.StringIndexOutOfBoundsException" -> "StringIndexOutOfBoundsException",
    "java.util.NoSuchElementException"      -> "NoSuchElementException",
    "java.util.ConcurrentModificationException" -> "ConcurrentModificationException",
  )

  /** Replace `$` with `_`, escape Python keywords. Single source of
   *  truth for both the runtime prelude (when it references a Scala
   *  class by Python identifier) and `PyIREmitter` (when it picks the
   *  emit-time identifier for a class def).
   */
  def sanitizeIdent(s: String): String =
    val cleaned = s.replace('$', '_')
    if PythonKeywords.contains(cleaned) then cleaned + "_"
    else cleaned

  /** Python identifier for a Scala class. Mirrors the rule the
   *  emitter applies in `PyIREmitter`:
   *
   *  - Names in [[PythonReservedShortNames]] take their explicit alias.
   *  - Classes in [[providedClasses]] keep their simple name so the
   *    runtime prelude's hand-written declarations (`class Comparable`,
   *    `class _scpy_Object`, etc.) line up with downstream references
   *    from the linker-emitted bundle.
   *  - Everything else uses the mangled FQN
   *    (`scala_collection_immutable_List`) to avoid simple-name
   *    collisions across packages.
   *
   *  Co-located with [[providedClasses]] so the simple-vs-mangled
   *  decision and the data driving it stay in one place — moving a
   *  class into or out of [[providedClasses]] automatically flips the
   *  identifier shape everywhere it's used.
   */
  def classIdentifier(name: PyClassName): String =
    PythonReservedShortNames.get(name.nameString) match
      case Some(alias) => alias
      case None =>
        if providedClass(name).isDefined then sanitizeIdent(name.simpleName)
        else name.segments.map(sanitizeIdent).mkString("_")

  // -- Name DSL for the runtime prelude --------------------------------
  //
  // The prelude carries hundreds of mangled identifiers that mirror the
  // encoder rules in `PyNames` and `PyTypes`. To keep the prelude in
  // sync with the encoder, every literal mangled name in `content` is
  // produced by a helper below — encoder changes propagate by recompile.

  /** Encoded method name. `<init>` → `__init__`, `<clinit>` →
   *  `_scpy_clinit`, dunders → bare name, otherwise
   *  `<simple>__<paramRefs>__<resultRef>`. */
  private def m(name: String, params: PyTypeRef*)(result: PyTypeRef): String =
    PyMethodName(PySimpleMethodName(name), params.toList, result).encoded

  /** Encoded field name. Public → simple name, private →
   *  `_scpy_f_<owner>__<name>`. */
  private def f(owner: PyClassName, name: String, isPrivate: Boolean = false): String =
    PyFieldName(owner, PySimpleFieldName(name), isPrivate).encoded

  /** Module-singleton variable name `_scpy_mod_<sanitizedFqn>_`. Mirrors
   *  `PyIREmitter.moduleVarName` so the prelude can reference modules
   *  without depending on the emitter. */
  private def mod(owner: PyClassName): String =
    s"_scpy_mod_${owner.segments.map(sanitizeIdent).mkString("_")}_"

  // Type-ref shortcuts. Only refs hit ≥ 4× across the prelude.
  private val ObjRef = PyClassRef(PyClassName.ObjectClass)
  private val StrRef = PyClassRef(PyClassName.StringClass)
  private val ClsRef = PyClassRef(PyClassName.ClassClass)

  // Primitive shortcuts — match the encoded letter so call sites read
  // like the encoded string (`m("apply", I, I)(Z)` ↔ `apply__I_I__Z`).
  private val V  = PyPrimRef.VoidRef
  private val I  = PyPrimRef.IntRef
  private val Z  = PyPrimRef.BooleanRef
  private val J  = PyPrimRef.LongRef
  private val F  = PyPrimRef.FloatRef
  private val D  = PyPrimRef.DoubleRef
  private val C  = PyPrimRef.CharRef
  private val B  = PyPrimRef.ByteRef
  private val Sh = PyPrimRef.ShortRef

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
    raw"""|# Scala.py runtime (generated by the Scala 3 Python backend)
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
       |        ${mod(PyClassName("java.lang.ThrowablesSupport$"))}.${m("throwIllegalMonitorState")(V)}()
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
       |    def ${m("getClass")(ClsRef)}(self):
       |        return _scpy_class_of_instance(self)
       |
       |    def ${m("toString")(StrRef)}(self):
       |        # Mirror `Object.toString` JVM semantics: virtual dispatch
       |        # through `hashCode()` (encoded as `__hash__` here), not the
       |        # identity hash. Value classes and case classes override
       |        # `__hash__`, so this picks up their wrapped-value hash and
       |        # produces e.g. `L@0` instead of `L@<python id>`.
       |        return self.${m("getClass")(ClsRef)}().${m("getName")(StrRef)}() + "@" + _builtins.format(self.__hash__() & 0xFFFFFFFF, "x")
       |
       |    def __str__(self):
       |        return self.${m("toString")(StrRef)}()
       |
       |    def ${m("wait")(V)}(self):
       |        return _scpy_object_wait(self)
       |
       |    def ${m("wait", J)(V)}(self, millis):
       |        timeout = None if millis == 0 else millis / 1000.0
       |        return _scpy_object_wait(self, timeout)
       |
       |    def ${m("wait", J, I)(V)}(self, millis, nanos):
       |        total_nanos = millis * 1000000 + nanos
       |        timeout = None if total_nanos == 0 else total_nanos / 1000000000.0
       |        return _scpy_object_wait(self, timeout)
       |
       |    def ${m("notify")(V)}(self):
       |        return _scpy_object_notify(self)
       |
       |    def ${m("notifyAll")(V)}(self):
       |        return _scpy_object_notify_all(self)
       |
       |_scpy_class_registry = {}
       |_scpy_unset = object()
       |
       |def _scpy_reflective_no_such(short_name, message):
       |    # Reflection stubs raise these JDK exceptions so callers
       |    # using `try { ... } catch (NoSuchFieldException) { ... }`
       |    # see the expected exception type. Class lookup is deferred
       |    # to runtime: pylib's exception classes may not be bound at
       |    # the time the prelude string is parsed, and DCE may pick
       |    # different names depending on classpath shape.
       |    exc_cls = globals().get("java_lang_" + short_name)
       |    if exc_cls is None:
       |        exc_cls = globals().get(short_name)
       |    if exc_cls is not None:
       |        try:
       |            return exc_cls(message)
       |        except Exception:
       |            try:
       |                return exc_cls(message, None, True, True)
       |            except Exception:
       |                pass
       |    return Exception(short_name + ": " + str(message))
       |
       |# Prefix used by every "JVM reflection is not supported" message.
       |# Tests assert against this prefix; do not change without updating
       |# `tests/pos-py/reflection-unsupported.scala` and any reachability
       |# unit tests that pin the message shape.
       |_SCPY_REFLECTION_UNSUPPORTED_PREFIX = (
       |    "JVM reflection is not supported in the -scalapy backend"
       |)
       |
       |def _scpy_reflection_unsupported(method_name):
       |    # Build an UnsupportedOperationException that callers can
       |    # `catch UnsupportedOperationException`. Lookup is deferred to
       |    # runtime for the same reason as `_scpy_reflective_no_such`:
       |    # the prelude is parsed before pylib classes are bound.
       |    message = (
       |        _SCPY_REFLECTION_UNSUPPORTED_PREFIX
       |        + " (Class." + method_name + "). "
       |        + "Excludelist the test or remove the reflective call; see "
       |        + "notes/wave5-worklist/01-reflection-unsupported-and-blacklist.md."
       |    )
       |    exc_cls = globals().get("java_lang_UnsupportedOperationException")
       |    if exc_cls is None:
       |        exc_cls = globals().get("UnsupportedOperationException")
       |    if exc_cls is not None:
       |        try:
       |            return exc_cls(message)
       |        except Exception:
       |            try:
       |                return exc_cls(message, None, True, True)
       |            except Exception:
       |                pass
       |    return Exception("UnsupportedOperationException: " + message)
       |
       |class _scpy_Class(_scpy_Object):
       |    def __init__(self, name, kind, component_type=None, py_type=None, simple_name=None, jvm_name=None):
       |        self._scpy_name = name
       |        self._scpy_kind = kind
       |        self._scpy_component_type = component_type
       |        self._scpy_py_type = py_type
       |        self._scpy_superclass_name = None
       |        self._scpy_interface_names = ()
       |        # Original Scala simple name (without `$$` module-suffix or
       |        # outer-class prefix). Codegen supplies this from the source
       |        # symbol; runtime-registered helper classes leave it None and
       |        # `_scpy_simple_name_of` falls back to a heuristic on `_scpy_name`.
       |        self._scpy_simple_name = simple_name
       |        # JVM-style dotted full name (e.g. `Foo$$$$anon$$1`,
       |        # `pkg.Outer$$Inner`). Codegen records this for user classes;
       |        # runtime-registered helper classes (e.g. `java.lang.Object`)
       |        # already use JVM-shaped strings for `_scpy_name`, so they
       |        # leave `_scpy_jvm_name` unset and `getName()` falls back to
       |        # `_scpy_name`.
       |        self._scpy_jvm_name = jvm_name
       |
       |    def ${m("getName")(StrRef)}(self):
       |        # Prefer the JVM-shaped name when codegen recorded one so
       |        # `getClass.getName` matches what the JVM backend emits
       |        # (`Foo$$$$anon$$1`) instead of the Python-encoded form
       |        # (`Foo__anon_1`). The fallback to `_scpy_name` covers
       |        # array classes (descriptor strings) and runtime helpers.
       |        if self._scpy_jvm_name is not None:
       |            return self._scpy_jvm_name
       |        return self._scpy_name
       |
       |    def ${m("getSimpleName")(StrRef)}(self):
       |        if self._scpy_kind == "array":
       |            comp = self._scpy_component_type
       |            if comp is None:
       |                return "[]"
       |            return comp.${m("getSimpleName")(StrRef)}() + "[]"
       |        # Prefer the JVM-shaped name when codegen recorded one:
       |        # parsing on the encoded form (`_scpy_name`) would split on
       |        # `_` boundaries that the encoder introduced for `$$`,
       |        # producing wrong names like `1` for `Foo__anon_1`.
       |        name = self._scpy_jvm_name if self._scpy_jvm_name is not None else self._scpy_name
       |        # Strip the outer-class prefix (after the last `$$`) for
       |        # nested types, then the package prefix (after the last `.`).
       |        dollar = name.rfind("$$")
       |        if dollar >= 0:
       |            name = name[dollar + 1:]
       |        else:
       |            dot = name.rfind(".")
       |            if dot >= 0:
       |                name = name[dot + 1:]
       |        return name
       |
       |    def ${m("getSuperclass")(ClsRef)}(self):
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
       |    def ${m("getInterfaces")(PyArrayRef(ClsRef, 1))}(self):
       |        if self._scpy_kind == "array":
       |            names = ("java.lang.Cloneable", "java.io.Serializable")
       |        else:
       |            names = self._scpy_interface_names
       |        return _scpy_array_value(
       |            _scpy_class_of_name("java.lang.Class"),
       |            [_scpy_class_of_name(name) for name in names],
       |        )
       |
       |    def ${m("getComponentType")(ClsRef)}(self):
       |        if self._scpy_kind == "array":
       |            return self._scpy_component_type
       |        return None
       |
       |    def ${m("isPrimitive")(Z)}(self):
       |        return self._scpy_kind == "primitive"
       |
       |    def ${m("isInterface")(Z)}(self):
       |        return self._scpy_kind == "interface"
       |
       |    def ${m("isArray")(Z)}(self):
       |        return self._scpy_kind == "array"
       |
       |    def ${m("isInstance", ObjRef)(Z)}(self, value):
       |        return _scpy_is_instance(value, self)
       |
       |    def ${m("isAssignableFrom", ClsRef)(Z)}(self, other):
       |        return _scpy_is_assignable(self, other)
       |
       |    def ${m("getClassLoader")(PyClassRef(PyClassName("java.lang.ClassLoader")))}(self):
       |        if self._scpy_kind == "primitive":
       |            return None
       |        return _scpy_system_class_loader
       |
       |    # Resource lookup is not wired up to a real classpath. Returning
       |    # `None` matches what the JVM does for absent resources, which is
       |    # the path most callers (e.g. `scala.util.Properties.scalaProps`
       |    # via `getResourceAsStream("/library.properties")`) already
       |    # handle by falling through to defaults.
       |    def ${m("getResourceAsStream", StrRef)(PyClassRef(PyClassName("java.io.InputStream")))}(self, name):
       |        return None
       |
       |    def ${m("getResource", StrRef)(PyClassRef(PyClassName("java.net.URL")))}(self, name):
       |        return None
       |
       |    # --- Reflection stubs ---
       |    # JVM-style reflective metadata (declared field/method/
       |    # constructor lists, generic signatures, annotations, enum-
       |    # constants tables) is not preserved through PyIR and code
       |    # generation. Per Wave 5 priority #1
       |    # (notes/wave5-worklist/01-reflection-unsupported-and-blacklist.md)
       |    # we raise an explicit `UnsupportedOperationException` for
       |    # these methods rather than returning fabricated empty arrays
       |    # / `None` (which used to surface as confusing
       |    # `AssertionError`s in user code that consumed the result).
       |    # Cheap intrinsic ops above (`getName`, `getSimpleName`,
       |    # `isArray`, `getSuperclass`, etc.) remain supported.
       |    def ${m("getDeclaredFields")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Field")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getDeclaredFields")
       |
       |    def ${m("getFields")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Field")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getFields")
       |
       |    def ${m("getDeclaredMethods")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Method")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getDeclaredMethods")
       |
       |    def ${m("getMethods")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Method")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getMethods")
       |
       |    def ${m("getDeclaredConstructors")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Constructor")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getDeclaredConstructors")
       |
       |    def ${m("getConstructors")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Constructor")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getConstructors")
       |
       |    def ${m("getDeclaredClasses")(PyArrayRef(ClsRef, 1))}(self):
       |        raise _scpy_reflection_unsupported("getDeclaredClasses")
       |
       |    def ${m("getClasses")(PyArrayRef(ClsRef, 1))}(self):
       |        raise _scpy_reflection_unsupported("getClasses")
       |
       |    def ${m("getGenericInterfaces")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Type")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getGenericInterfaces")
       |
       |    def ${m("getGenericSuperclass")(PyClassRef(PyClassName("java.lang.reflect.Type")))}(self):
       |        raise _scpy_reflection_unsupported("getGenericSuperclass")
       |
       |    def ${m("getTypeParameters")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.TypeVariable")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getTypeParameters")
       |
       |    def ${m("getDeclaredField", StrRef)(PyClassRef(PyClassName("java.lang.reflect.Field")))}(self, name):
       |        raise _scpy_reflection_unsupported("getDeclaredField")
       |
       |    def ${m("getField", StrRef)(PyClassRef(PyClassName("java.lang.reflect.Field")))}(self, name):
       |        raise _scpy_reflection_unsupported("getField")
       |
       |    def ${m("getDeclaredMethod", StrRef, PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Method")))}(self, name, *_args):
       |        raise _scpy_reflection_unsupported("getDeclaredMethod")
       |
       |    def ${m("getMethod", StrRef, PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Method")))}(self, name, *_args):
       |        raise _scpy_reflection_unsupported("getMethod")
       |
       |    def ${m("getDeclaredConstructor", PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}(self, *_args):
       |        raise _scpy_reflection_unsupported("getDeclaredConstructor")
       |
       |    def ${m("getConstructor", PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}(self, *_args):
       |        raise _scpy_reflection_unsupported("getConstructor")
       |
       |    def ${m("getEnclosingMethod")(PyClassRef(PyClassName("java.lang.reflect.Method")))}(self):
       |        raise _scpy_reflection_unsupported("getEnclosingMethod")
       |
       |    def ${m("getEnclosingConstructor")(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}(self):
       |        raise _scpy_reflection_unsupported("getEnclosingConstructor")
       |
       |    def ${m("getEnclosingClass")(ClsRef)}(self):
       |        raise _scpy_reflection_unsupported("getEnclosingClass")
       |
       |    def ${m("getDeclaringClass")(ClsRef)}(self):
       |        raise _scpy_reflection_unsupported("getDeclaringClass")
       |
       |    def ${m("getEnumConstants")(PyArrayRef(ObjRef, 1))}(self):
       |        raise _scpy_reflection_unsupported("getEnumConstants")
       |
       |    def ${m("getModifiers")(I)}(self):
       |        return 0
       |
       |    def ${m("getCanonicalName")(StrRef)}(self):
       |        # JVM canonical name elides `$$` separators and is undefined
       |        # for anonymous/local classes. Approximate with `getName()`
       |        # so codegen's recorded JVM name shows through.
       |        if self._scpy_jvm_name is not None:
       |            return self._scpy_jvm_name
       |        return self._scpy_name
       |
       |    def ${m("getTypeName")(StrRef)}(self):
       |        if self._scpy_jvm_name is not None:
       |            return self._scpy_jvm_name
       |        return self._scpy_name
       |
       |    def ${m("getPackageName")(StrRef)}(self):
       |        # Use the JVM-shaped name when present so the package
       |        # qualifier is split at the right `.` boundary; the
       |        # encoded `_scpy_name` shares the same package prefix
       |        # but the inner-class form differs only in the final
       |        # segment, where `getPackageName` doesn't look anyway.
       |        name = self._scpy_jvm_name if self._scpy_jvm_name is not None else self._scpy_name
       |        dot = name.rfind(".")
       |        return name[:dot] if dot >= 0 else ""
       |
       |    def ${m("getPackage")(PyClassRef(PyClassName("java.lang.Package")))}(self):
       |        return None
       |
       |    def ${m("getSigners")(PyArrayRef(ObjRef, 1))}(self):
       |        return None
       |
       |    def ${m("getNestHost")(ClsRef)}(self):
       |        return self
       |
       |    def ${m("getNestMembers")(PyArrayRef(ClsRef, 1))}(self):
       |        return _scpy_array_value(_scpy_class_of_name("java.lang.Class"), [self])
       |
       |    def ${m("getPermittedSubclasses")(PyArrayRef(ClsRef, 1))}(self):
       |        return None
       |
       |    def ${m("getRecordComponents")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.RecordComponent")), 1))}(self):
       |        return None
       |
       |    def ${m("getProtectionDomain")(PyClassRef(PyClassName("java.security.ProtectionDomain")))}(self):
       |        return None
       |
       |    # Annotation reflection: per the Wave 5 policy, all of these
       |    # raise UnsupportedOperationException too. The runtime cannot
       |    # reconstruct annotation instances from PyIR.
       |    def ${m("getAnnotation", ClsRef)(PyClassRef(PyClassName("java.lang.annotation.Annotation")))}(self, ann_class):
       |        raise _scpy_reflection_unsupported("getAnnotation")
       |
       |    def ${m("getAnnotationsByType", ClsRef)(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}(self, ann_class):
       |        raise _scpy_reflection_unsupported("getAnnotationsByType")
       |
       |    def ${m("getAnnotations")(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getAnnotations")
       |
       |    def ${m("getDeclaredAnnotations")(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}(self):
       |        raise _scpy_reflection_unsupported("getDeclaredAnnotations")
       |
       |    def ${m("getDeclaredAnnotation", ClsRef)(PyClassRef(PyClassName("java.lang.annotation.Annotation")))}(self, ann_class):
       |        raise _scpy_reflection_unsupported("getDeclaredAnnotation")
       |
       |    def ${m("getDeclaredAnnotationsByType", ClsRef)(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}(self, ann_class):
       |        raise _scpy_reflection_unsupported("getDeclaredAnnotationsByType")
       |
       |    def ${m("isAnnotationPresent", ClsRef)(Z)}(self, ann_class):
       |        raise _scpy_reflection_unsupported("isAnnotationPresent")
       |
       |    def ${m("isAnonymousClass")(Z)}(self):
       |        return False
       |
       |    def ${m("isLocalClass")(Z)}(self):
       |        return False
       |
       |    def ${m("isMemberClass")(Z)}(self):
       |        return False
       |
       |    def ${m("isSynthetic")(Z)}(self):
       |        return False
       |
       |    def ${m("isEnum")(Z)}(self):
       |        return False
       |
       |    def ${m("isAnnotation")(Z)}(self):
       |        return False
       |
       |    def ${m("isRecord")(Z)}(self):
       |        return False
       |
       |    def ${m("isHidden")(Z)}(self):
       |        return False
       |
       |    def ${m("isSealed")(Z)}(self):
       |        return False
       |
       |    def ${m("desiredAssertionStatus")(Z)}(self):
       |        return False
       |
       |    def ${m("asSubclass", ClsRef)(ClsRef)}(self, other):
       |        return self
       |
       |    def ${m("cast", ObjRef)(ObjRef)}(self, value):
       |        return value
       |
       |    def ${m("newInstance")(ObjRef)}(self):
       |        # `Class.newInstance()` is part of the JVM-reflection
       |        # surface; the Python backend does not support it (see
       |        # notes/wave5-worklist/01-reflection-unsupported-and-blacklist.md).
       |        raise _scpy_reflection_unsupported("newInstance")
       |
       |    def ${m("toGenericString")(StrRef)}(self):
       |        return self.${m("toString")(StrRef)}()
       |
       |    def ${m("toString")(StrRef)}(self):
       |        # `Class.toString` reports the user-visible name (JVM-shaped
       |        # when codegen recorded one), not the encoded `_scpy_name`.
       |        # Mirrors `getName()` so `println(getClass)` prints e.g.
       |        # `class Foo$$$$anon$$1` rather than `class Foo__anon_1`.
       |        display_name = self._scpy_jvm_name if self._scpy_jvm_name is not None else self._scpy_name
       |        if self._scpy_kind == "primitive":
       |            return display_name
       |        if self._scpy_kind == "interface":
       |            return "interface " + display_name
       |        return "class " + display_name
       |
       |    def __str__(self):
       |        return self.${m("toString")(StrRef)}()
       |
       |    def __getattr__(self, name):
       |        # NOTE: prefix-based dispatch — longer prefixes MUST be
       |        # checked first. `getDeclaredFields` shares a prefix with
       |        # `getDeclaredField`, etc. The plural/singular pairs are
       |        # ordered so the plural matches first.
       |        if name.startswith("getSimpleName"):
       |            return self.${m("getSimpleName")(StrRef)}
       |        if name.startswith("getName"):
       |            return self.${m("getName")(StrRef)}
       |        if name.startswith("getCanonicalName"):
       |            return self.${m("getCanonicalName")(StrRef)}
       |        if name.startswith("getTypeName"):
       |            return self.${m("getTypeName")(StrRef)}
       |        if name.startswith("getPackageName"):
       |            return self.${m("getPackageName")(StrRef)}
       |        if name.startswith("getPackage"):
       |            return self.${m("getPackage")(PyClassRef(PyClassName("java.lang.Package")))}
       |        if name.startswith("getSuperclass"):
       |            return self.${m("getSuperclass")(ClsRef)}
       |        if name.startswith("getGenericSuperclass"):
       |            return self.${m("getGenericSuperclass")(PyClassRef(PyClassName("java.lang.reflect.Type")))}
       |        if name.startswith("getGenericInterfaces"):
       |            return self.${m("getGenericInterfaces")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Type")), 1))}
       |        if name.startswith("getInterfaces"):
       |            return self.${m("getInterfaces")(PyArrayRef(ClsRef, 1))}
       |        if name.startswith("getComponentType"):
       |            return self.${m("getComponentType")(ClsRef)}
       |        if name.startswith("getTypeParameters"):
       |            return self.${m("getTypeParameters")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.TypeVariable")), 1))}
       |        if name.startswith("getDeclaredFields"):
       |            return self.${m("getDeclaredFields")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Field")), 1))}
       |        if name.startswith("getDeclaredField"):
       |            return self.${m("getDeclaredField", StrRef)(PyClassRef(PyClassName("java.lang.reflect.Field")))}
       |        if name.startswith("getFields"):
       |            return self.${m("getFields")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Field")), 1))}
       |        if name.startswith("getField"):
       |            return self.${m("getField", StrRef)(PyClassRef(PyClassName("java.lang.reflect.Field")))}
       |        if name.startswith("getDeclaredMethods"):
       |            return self.${m("getDeclaredMethods")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Method")), 1))}
       |        if name.startswith("getDeclaredMethod"):
       |            return self.${m("getDeclaredMethod", StrRef, PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Method")))}
       |        if name.startswith("getMethods"):
       |            return self.${m("getMethods")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Method")), 1))}
       |        if name.startswith("getMethod"):
       |            return self.${m("getMethod", StrRef, PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Method")))}
       |        if name.startswith("getDeclaredConstructors"):
       |            return self.${m("getDeclaredConstructors")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Constructor")), 1))}
       |        if name.startswith("getDeclaredConstructor"):
       |            return self.${m("getDeclaredConstructor", PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}
       |        if name.startswith("getConstructors"):
       |            return self.${m("getConstructors")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.Constructor")), 1))}
       |        if name.startswith("getConstructor"):
       |            return self.${m("getConstructor", PyArrayRef(ClsRef, 1))(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}
       |        if name.startswith("getDeclaredClasses"):
       |            return self.${m("getDeclaredClasses")(PyArrayRef(ClsRef, 1))}
       |        if name.startswith("getClasses"):
       |            return self.${m("getClasses")(PyArrayRef(ClsRef, 1))}
       |        if name.startswith("getEnclosingMethod"):
       |            return self.${m("getEnclosingMethod")(PyClassRef(PyClassName("java.lang.reflect.Method")))}
       |        if name.startswith("getEnclosingConstructor"):
       |            return self.${m("getEnclosingConstructor")(PyClassRef(PyClassName("java.lang.reflect.Constructor")))}
       |        if name.startswith("getEnclosingClass"):
       |            return self.${m("getEnclosingClass")(ClsRef)}
       |        if name.startswith("getDeclaringClass"):
       |            return self.${m("getDeclaringClass")(ClsRef)}
       |        if name.startswith("getEnumConstants"):
       |            return self.${m("getEnumConstants")(PyArrayRef(ObjRef, 1))}
       |        if name.startswith("getModifiers"):
       |            return self.${m("getModifiers")(I)}
       |        if name.startswith("getNestHost"):
       |            return self.${m("getNestHost")(ClsRef)}
       |        if name.startswith("getNestMembers"):
       |            return self.${m("getNestMembers")(PyArrayRef(ClsRef, 1))}
       |        if name.startswith("getPermittedSubclasses"):
       |            return self.${m("getPermittedSubclasses")(PyArrayRef(ClsRef, 1))}
       |        if name.startswith("getRecordComponents"):
       |            return self.${m("getRecordComponents")(PyArrayRef(PyClassRef(PyClassName("java.lang.reflect.RecordComponent")), 1))}
       |        if name.startswith("getProtectionDomain"):
       |            return self.${m("getProtectionDomain")(PyClassRef(PyClassName("java.security.ProtectionDomain")))}
       |        if name.startswith("getSigners"):
       |            return self.${m("getSigners")(PyArrayRef(ObjRef, 1))}
       |        if name.startswith("getDeclaredAnnotations"):
       |            return self.${m("getDeclaredAnnotations")(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}
       |        if name.startswith("getDeclaredAnnotationsByType"):
       |            return self.${m("getDeclaredAnnotationsByType", ClsRef)(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}
       |        if name.startswith("getDeclaredAnnotation"):
       |            return self.${m("getDeclaredAnnotation", ClsRef)(PyClassRef(PyClassName("java.lang.annotation.Annotation")))}
       |        if name.startswith("getAnnotationsByType"):
       |            return self.${m("getAnnotationsByType", ClsRef)(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}
       |        if name.startswith("getAnnotations"):
       |            return self.${m("getAnnotations")(PyArrayRef(PyClassRef(PyClassName("java.lang.annotation.Annotation")), 1))}
       |        if name.startswith("getAnnotation"):
       |            return self.${m("getAnnotation", ClsRef)(PyClassRef(PyClassName("java.lang.annotation.Annotation")))}
       |        if name.startswith("isAnnotationPresent"):
       |            return self.${m("isAnnotationPresent", ClsRef)(Z)}
       |        if name.startswith("isAnnotation"):
       |            return self.${m("isAnnotation")(Z)}
       |        if name.startswith("isAnonymousClass"):
       |            return self.${m("isAnonymousClass")(Z)}
       |        if name.startswith("isLocalClass"):
       |            return self.${m("isLocalClass")(Z)}
       |        if name.startswith("isMemberClass"):
       |            return self.${m("isMemberClass")(Z)}
       |        if name.startswith("isSynthetic"):
       |            return self.${m("isSynthetic")(Z)}
       |        if name.startswith("isEnum"):
       |            return self.${m("isEnum")(Z)}
       |        if name.startswith("isRecord"):
       |            return self.${m("isRecord")(Z)}
       |        if name.startswith("isHidden"):
       |            return self.${m("isHidden")(Z)}
       |        if name.startswith("isSealed"):
       |            return self.${m("isSealed")(Z)}
       |        if name.startswith("isPrimitive"):
       |            return self.${m("isPrimitive")(Z)}
       |        if name.startswith("isInterface"):
       |            return self.${m("isInterface")(Z)}
       |        if name.startswith("isArray"):
       |            return self.${m("isArray")(Z)}
       |        if name.startswith("isInstance"):
       |            return self.${m("isInstance", ObjRef)(Z)}
       |        if name.startswith("isAssignableFrom"):
       |            return self.${m("isAssignableFrom", ClsRef)(Z)}
       |        if name.startswith("desiredAssertionStatus"):
       |            return self.${m("desiredAssertionStatus")(Z)}
       |        if name.startswith("asSubclass"):
       |            return self.${m("asSubclass", ClsRef)(ClsRef)}
       |        if name.startswith("cast"):
       |            return self.${m("cast", ObjRef)(ObjRef)}
       |        if name.startswith("newInstance"):
       |            return self.${m("newInstance")(ObjRef)}
       |        if name.startswith("getClassLoader"):
       |            return self.${m("getClassLoader")(PyClassRef(PyClassName("java.lang.ClassLoader")))}
       |        if name.startswith("getResourceAsStream"):
       |            return self.${m("getResourceAsStream", StrRef)(PyClassRef(PyClassName("java.io.InputStream")))}
       |        if name.startswith("getResource"):
       |            return self.${m("getResource", StrRef)(PyClassRef(PyClassName("java.net.URL")))}
       |        if name.startswith("toGenericString"):
       |            return self.${m("toGenericString")(StrRef)}
       |        if name.startswith("toString"):
       |            return self.${m("toString")(StrRef)}
       |        raise AttributeError(name)
       |
       |Class = _scpy_Class
       |
       |class _scpy_ClassModule(_scpy_Object):
       |    # Stand-in for `java.lang.Class$$` (the static-method receiver
       |    # for `Class.forName`). The Scala backend lowers
       |    # `Class.forName(name)` to a static call whose Python target is
       |    # `${mod(PyClassName("java.lang.Class"))}.forName__...(name)`. There is no
       |    # Scala companion source for `java.lang.Class`, so without this
       |    # the bundle would reference an undefined module variable.
       |    # Aliased below as `${mod(PyClassName("java.lang.Class"))}` (and the
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
       |            # 4-arg `Throwable(message, cause, enableSuppression,
       |            # writableStackTrace)`. The Scala-emitted class's
       |            # `__init__(self, *args)` arity-dispatches to the
       |            # right ctor helper.
       |            raise exc_cls(name, None, True, True)
       |        # Last-resort fallback for tests where ClassNotFoundException
       |        # has been DCE'd out of the bundle.
       |        raise Exception("ClassNotFoundException: " + str(name))
       |
       |    def ${m("forName", StrRef)(ClsRef)}(self, name):
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def ${m("forName", StrRef, Z, PyClassRef(PyClassName("java.lang.ClassLoader")))(ClsRef)}(self, name, initialize, loader):
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def ${m("forName", StrRef, PyClassRef(PyClassName("java.lang.Module")))(ClsRef)}(self, name, module):
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
       |        # Cache once: is this a primitive `char[]`? If so, reads
       |        # auto-box raw ints into `_scpy_Char` so iterators that
       |        # forward through `ScalaRunTime.array_apply` (e.g.
       |        # `ArrayOps.ArrayIterator.next()`) hand off a value whose
       |        # `_scpy_to_str` renders the codepoint glyph instead of
       |        # the numeric form. Other primitive arrays don't need
       |        # this — `str(int)`/`str(float)` already match Scala.
       |        comp = clazz._scpy_component_type if clazz is not None else None
       |        self._scpy_is_char_array = (
       |            comp is not None
       |            and comp._scpy_kind == "primitive"
       |            and comp._scpy_name == "char"
       |        )
       |
       |    def __getitem__(self, idx):
       |        v = list.__getitem__(self, idx)
       |        if self._scpy_is_char_array and isinstance(v, int) and not isinstance(v, _scpy_Char) and not isinstance(v, bool):
       |            return _scpy_Char(v)
       |        return v
       |
       |    def ${m("getClass")(ClsRef)}(self):
       |        return self._scpy_class
       |
       |    def ${m("clone")(ObjRef)}(self):
       |        return _scpy_Array(self, self._scpy_class)
       |
       |    def ${m("toString")(StrRef)}(self):
       |        # Mirror JVM `Object.toString` on an array: descriptor
       |        # of the runtime class joined with `@<lower-hex identity hash>`.
       |        # For an `Array[Unit]` whose element class is
       |        # `scala.runtime.BoxedUnit`, this yields strings shaped like
       |        # `[Lscala.runtime.BoxedUnit;@1a2b3c`. The descriptor for an
       |        # array class is just `_scpy_name` (e.g. `[Lscala.runtime.BoxedUnit;`),
       |        # which is exactly what `_scpy_descriptor_for_class` returns
       |        # when `_scpy_kind == "array"`.
       |        #
       |        # We deliberately don't reuse `_scpy_Object.toString` here:
       |        # `_scpy_Array` extends `list`, and `list.__hash__ is None`,
       |        # so dispatching through `self.__hash__()` would TypeError.
       |        # JVM identity hash is `id(self) & 0xFFFFFFFF`.
       |        return _scpy_descriptor_for_class(self._scpy_class) + "@" + _builtins.format(_builtins.id(self) & 0xFFFFFFFF, "x")
       |
       |    # JVM `Object.hashCode` and `Object.equals` are exposed under
       |    # both their mangled (`${m("hashCode")(I)}` / `${m("equals", ObjRef)(Z)}`)
       |    # and Python-dunder (`__hash__` / `__eq__`) shapes. The encoder
       |    # rewrites Scala-source `arr.hashCode` to `arr.__hash__()` and
       |    # `arr.equals(x)` to `arr.__eq__(x)` (see `PyEncoding.specialMethodNameOf`),
       |    # so the dunder forms are what user code actually invokes; the
       |    # mangled forms are pinned for parity with `_scpy_Char` (which
       |    # carries both shapes) and to handle any post-erasure bridge
       |    # call that bypasses the encoder rewrite.
       |    #
       |    # Critically, `list.__hash__ is None` (lists are unhashable in
       |    # Python), so without our own `__hash__` override the dunder
       |    # call would TypeError. `_scpy_any_hash_code` (the `Any#hashCode`
       |    # router used when the static receiver type is `AnyRef`/`Object`)
       |    # also falls through to `x.__hash__()` for non-primitive
       |    # receivers, so providing `__hash__` here covers that path too.
       |
       |    def __hash__(self):
       |        h = _builtins.id(self) & 0xFFFFFFFF
       |        if h >= 0x80000000:
       |            h -= 0x100000000
       |        return h
       |
       |    def ${m("hashCode")(I)}(self):
       |        return self.__hash__()
       |
       |    def __eq__(self, other):
       |        # JVM `Object.equals` is reference identity by default.
       |        # `_scpy_Array` doesn't override structural equality, so we
       |        # explicitly opt out of the inherited `list.__eq__` (which
       |        # would compare element-by-element).
       |        return self is other
       |
       |    def __ne__(self, other):
       |        return self is not other
       |
       |    def ${m("equals", ObjRef)(Z)}(self, other):
       |        return self is other
       |
       |def _scpy_class_of_name(name, kind="class"):
       |    clazz = _scpy_class_registry.get(name)
       |    if clazz is None:
       |        clazz = _scpy_Class(name, kind)
       |        _scpy_class_registry[name] = clazz
       |    return clazz
       |
       |def _scpy_register_class(py_type, name, kind="class", superclass_name=None, interface_names=(), component_type=None, simple_name=None, jvm_name=None):
       |    clazz = _scpy_class_registry.get(name)
       |    if clazz is None:
       |        clazz = _scpy_Class(name, kind, component_type, py_type, simple_name, jvm_name)
       |        _scpy_class_registry[name] = clazz
       |    else:
       |        clazz._scpy_kind = kind
       |        clazz._scpy_component_type = component_type
       |        if py_type is not None:
       |            clazz._scpy_py_type = py_type
       |        if simple_name is not None:
       |            clazz._scpy_simple_name = simple_name
       |        if jvm_name is not None:
       |            clazz._scpy_jvm_name = jvm_name
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
       |def _scpy_simple_name_of(value):
       |    # Returns the user-visible Scala simple name of `value`'s class
       |    # (e.g. `D1` for `object D1 extends Enumeration` defined inside
       |    # `object Test5`). When the registered class carries an explicit
       |    # `_scpy_simple_name` (codegen path), use it directly; otherwise
       |    # fall back to a heuristic on the encoded full name where
       |    # `$$` separators have already been mapped to `_`.
       |    clazz = _scpy_class_of_instance(value)
       |    name = clazz._scpy_simple_name
       |    if name is not None:
       |        return name
       |    return _scpy_simple_name_from_encoded(clazz._scpy_name)
       |
       |def _scpy_simple_name_from_encoded(name):
       |    if not name:
       |        return name
       |    # Drop trailing `_` that came from a Scala module-class `$$`
       |    # suffix.
       |    while name.endswith("_"):
       |        name = name[:-1]
       |    # Take the segment after the last package-separator `.`.
       |    dot = name.rfind(".")
       |    if dot >= 0:
       |        name = name[dot + 1:]
       |    # Take the segment after the last inner-class boundary, which
       |    # `PyEncoding.sanitizeName` mapped from `$$` to `_`.
       |    underscore = name.rfind("_")
       |    if underscore >= 0:
       |        name = name[underscore + 1:]
       |    return name
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
       |def _scpy_get_class(obj):
       |    # Codegen intercept for `Object.getClass()` on Any/Object/Matchable/
       |    # boxed/String receivers - avoids AttributeError on raw Python ints,
       |    # strs, floats, bools by dispatching via _scpy_class_of_instance.
       |    return _scpy_class_of_instance(obj)
       |
       |def _scpy_is_assignable(target, source):
       |    if target is None or source is None:
       |        return False
       |    if target is source:
       |        return True
       |    if target._scpy_kind == "primitive" or source._scpy_kind == "primitive":
       |        return False
       |    # Every non-primitive reference type is implicitly assignable to
       |    # `java.lang.Object`. Interfaces are registered with
       |    # `_scpy_superclass_name = None` (the JVM bakes the
       |    # `interface <: Object` edge into the type system rather than the
       |    # interface table), so the walk path below would otherwise miss
       |    # it — e.g. `Array[java.io.Serializable]` would not match
       |    # `case x: Array[AnyRef]` in `ArraySeq.unsafeWrapArray`.
       |    if target._scpy_name == "java.lang.Object":
       |        return True
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
       |    # Python raises `AttributeError` for `None.attr` / `None.method()`,
       |    # which is the JVM equivalent of `NullPointerException`. Scala
       |    # programs catch the latter (`case _: NullPointerException`,
       |    # `case _: RuntimeException`, `case _: Throwable`, ...). Narrow
       |    # the rewrite to AttributeErrors whose message matches the
       |    # CPython `'NoneType' object has no attribute '<name>'` shape so
       |    # that genuine facade/dynamic AttributeErrors (typo'd attribute
       |    # on a real Python object) are not silently swallowed.
       |    if isinstance(value, AttributeError):
       |        msg = _builtins.str(value)
       |        if "'NoneType' object" in msg:
       |            npe_cls = _scpy_class_of_name("java.lang.NullPointerException")
       |            if _scpy_is_assignable(clazz, npe_cls):
       |                return True
       |    # Scala tuples lower to native Python tuples. The class side
       |    # has no `_scpy_class` link to the tuple value, so `_scpy_is_instance`
       |    # cannot answer; resolve based on the registered class name.
       |    if isinstance(value, tuple):
       |        name = getattr(clazz, "_scpy_name", None)
       |        if name is None:
       |            return False
       |        if name == "scala.Tuple":
       |            return True
       |        if name == "scala.NonEmptyTuple":
       |            return len(value) > 0
       |        if name == "scala.EmptyTuple$":
       |            return len(value) == 0
       |        if name == "scala.runtime.TupleXXL":
       |            return len(value) > 22
       |        if name.startswith("scala.Tuple"):
       |            tail = name[len("scala.Tuple"):]
       |            if tail.isdigit():
       |                return len(value) == int(tail)
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
       |class Comparable(_scpy_Object):
       |    pass
       |
       |class Serializable(_scpy_Object):
       |    pass
       |""".stripMargin +
    raw"""|# Linker-only nominal stubs. Stdlib references them by name (some as
       |# bases — Stepper/Spliterator path), so Python must have a class to
       |# inherit from. Empty bodies — runtime never executes their methods.
       |#
       |# `VarHandle` is the one exception: the post-erasure `LazyVals` mini-
       |# phase emits per-lazy-val `VarHandle.compareAndSet(self, expected,
       |# replacement)` calls keyed on the underlying container field name.
       |# The Python backend can't replicate JVM `<clinit>`-time
       |# `MethodHandles.lookup().findVarHandle(...)` setup, so the emitter
       |# binds each `<container>_lzyHandle` field to a `VarHandle` instance
       |# carrying that container's field name and uses Python `getattr` /
       |# `setattr` to implement CAS. Single-threaded — the Python pos-py
       |# tests don't exercise the LazyVals races, and a future thread-safe
       |# variant can wrap the body in a `threading.RLock`.
       |class VarHandle(_scpy_Object):
       |    def __init__(self, field_name=None):
       |        self._scpy_field_name = field_name
       |    def ${m("compareAndSet", ObjRef, ObjRef, ObjRef)(Z)}(self, target, expected, replacement):
       |        current = _builtins.getattr(target, self._scpy_field_name, None)
       |        if current is expected:
       |            _builtins.setattr(target, self._scpy_field_name, replacement)
       |            return True
       |        return False
       |def _scpy_make_lazy_handle(field_name):
       |    return VarHandle(field_name)
       |class MethodHandles(_scpy_Object): pass
       |class MethodHandles_Lookup(_scpy_Object): pass
       |class AbstractStringBuilder(_scpy_Object): pass
       |class AccessibleObject(_scpy_Object):
       |    # Reflection support is intentionally limited; see
       |    # `notes/issue-reflection-class-introspection.md`. These
       |    # classes exist so user code that mentions them at compile
       |    # time links and so the array helpers in `_scpy_Class`
       |    # have a registered component class to point at.
       |    def __init__(self, *_args, **_kw):
       |        pass
       |    def ${m("setAccessible", Z)(V)}(self, flag):
       |        pass
       |    def ${m("isAccessible")(Z)}(self):
       |        return False
       |    def __getattr__(self, name):
       |        # Permissive fallback so unforeseen reflective probes
       |        # don't crash on AttributeError. Returns a no-op callable
       |        # that yields None / 0 / empty depending on suffix shape.
       |        if name.startswith("getName") or name.startswith("toGenericString") or name.startswith("toString"):
       |            return lambda *args, **kw: ""
       |        if name.startswith("getModifiers"):
       |            return lambda *args, **kw: 0
       |        if name.startswith("getDeclaringClass"):
       |            return lambda *args, **kw: None
       |        if name.startswith("getReturnType") or name.startswith("getType") or name.startswith("getGenericReturnType") or name.startswith("getGenericType"):
       |            return lambda *args, **kw: _scpy_class_of_name("java.lang.Object")
       |        if name.startswith("getParameterTypes") or name.startswith("getGenericParameterTypes"):
       |            return lambda *args, **kw: _scpy_array_value(_scpy_class_of_name("java.lang.Class"), [])
       |        if name.startswith("getExceptionTypes") or name.startswith("getGenericExceptionTypes"):
       |            return lambda *args, **kw: _scpy_array_value(_scpy_class_of_name("java.lang.Class"), [])
       |        if name.startswith("getDeclaredAnnotations") or name.startswith("getAnnotations") or name.startswith("getParameterAnnotations"):
       |            return lambda *args, **kw: _scpy_array_value(_scpy_class_of_name("java.lang.annotation.Annotation"), [])
       |        if name.startswith("getAnnotation") or name.startswith("getDeclaredAnnotation"):
       |            return lambda *args, **kw: None
       |        if name.startswith("invoke"):
       |            return lambda *args, **kw: None
       |        if name.startswith("get__") or name.startswith("set__"):
       |            return lambda *args, **kw: None
       |        if name.startswith("isVarArgs") or name.startswith("isAccessible") or name.startswith("isSynthetic") or name.startswith("isBridge") or name.startswith("isDefault"):
       |            return lambda *args, **kw: False
       |        raise AttributeError(name)
       |class Method(AccessibleObject): pass
       |class Field(AccessibleObject): pass
       |class Executable(AccessibleObject): pass
       |class Constructor(AccessibleObject): pass
       |# `java.lang.reflect.Type` and `TypeVariable` are interfaces; we
       |# just need a nominal placeholder so user code linking against
       |# them resolves.
       |class Type(_scpy_Object): pass
       |class TypeVariable(Type): pass
       |class Modifier(_scpy_Object):
       |    # Modifier flag constants and predicates as per JDK
       |    # `java.lang.reflect.Modifier`. Static-only API.
       |    PUBLIC       = 0x00000001
       |    PRIVATE      = 0x00000002
       |    PROTECTED    = 0x00000004
       |    STATIC       = 0x00000008
       |    FINAL        = 0x00000010
       |    SYNCHRONIZED = 0x00000020
       |    VOLATILE     = 0x00000040
       |    TRANSIENT    = 0x00000080
       |    NATIVE       = 0x00000100
       |    INTERFACE    = 0x00000200
       |    ABSTRACT     = 0x00000400
       |    STRICT       = 0x00000800
       |    @staticmethod
       |    def ${m("isPublic", I)(Z)}(mod):       return (mod & Modifier.PUBLIC) != 0
       |    @staticmethod
       |    def ${m("isPrivate", I)(Z)}(mod):      return (mod & Modifier.PRIVATE) != 0
       |    @staticmethod
       |    def ${m("isProtected", I)(Z)}(mod):    return (mod & Modifier.PROTECTED) != 0
       |    @staticmethod
       |    def ${m("isStatic", I)(Z)}(mod):       return (mod & Modifier.STATIC) != 0
       |    @staticmethod
       |    def ${m("isFinal", I)(Z)}(mod):        return (mod & Modifier.FINAL) != 0
       |    @staticmethod
       |    def ${m("isSynchronized", I)(Z)}(mod): return (mod & Modifier.SYNCHRONIZED) != 0
       |    @staticmethod
       |    def ${m("isVolatile", I)(Z)}(mod):     return (mod & Modifier.VOLATILE) != 0
       |    @staticmethod
       |    def ${m("isTransient", I)(Z)}(mod):    return (mod & Modifier.TRANSIENT) != 0
       |    @staticmethod
       |    def ${m("isNative", I)(Z)}(mod):       return (mod & Modifier.NATIVE) != 0
       |    @staticmethod
       |    def ${m("isInterface", I)(Z)}(mod):    return (mod & Modifier.INTERFACE) != 0
       |    @staticmethod
       |    def ${m("isAbstract", I)(Z)}(mod):     return (mod & Modifier.ABSTRACT) != 0
       |    @staticmethod
       |    def ${m("isStrict", I)(Z)}(mod):       return (mod & Modifier.STRICT) != 0
       |    @staticmethod
       |    def ${m("toString", I)(StrRef)}(mod):
       |        return ""
       |    def __getattr__(self, name):
       |        # Static-style dispatch for module-call shape
       |        # `${mod(PyClassName("java.lang.reflect.Modifier"))}.${m("isPublic", I)(Z)}`.
       |        if name.startswith("isPublic"):       return Modifier.${m("isPublic", I)(Z)}
       |        if name.startswith("isPrivate"):      return Modifier.${m("isPrivate", I)(Z)}
       |        if name.startswith("isProtected"):    return Modifier.${m("isProtected", I)(Z)}
       |        if name.startswith("isStatic"):       return Modifier.${m("isStatic", I)(Z)}
       |        if name.startswith("isFinal"):        return Modifier.${m("isFinal", I)(Z)}
       |        if name.startswith("isSynchronized"): return Modifier.${m("isSynchronized", I)(Z)}
       |        if name.startswith("isVolatile"):     return Modifier.${m("isVolatile", I)(Z)}
       |        if name.startswith("isTransient"):    return Modifier.${m("isTransient", I)(Z)}
       |        if name.startswith("isNative"):       return Modifier.${m("isNative", I)(Z)}
       |        if name.startswith("isInterface"):    return Modifier.${m("isInterface", I)(Z)}
       |        if name.startswith("isAbstract"):     return Modifier.${m("isAbstract", I)(Z)}
       |        if name.startswith("isStrict"):       return Modifier.${m("isStrict", I)(Z)}
       |        if name.startswith("toString"):       return Modifier.${m("toString", I)(StrRef)}
       |        raise AttributeError(name)
       |# Module singleton for `Modifier` so static-style emission resolves.
       |${mod(PyClassName("java.lang.reflect.Modifier"))}  = Modifier()
       |${mod(PyClassName("java.lang.reflect.Modifier$"))} = ${mod(PyClassName("java.lang.reflect.Modifier"))}
       |class InvocationTargetException(Exception):
       |    # Pylib's `java.lang.reflect.InvocationTargetException` extends
       |    # ReflectiveOperationException. The runtime never raises it
       |    # because we don't actually invoke methods reflectively, but
       |    # user code may catch or instantiate it.
       |    def __init__(self, cause=None, message=None, *_args):
       |        super().__init__(message if message is not None else (str(cause) if cause is not None else ""))
       |        self._scpy_cause = cause
       |    def ${m("getCause")(PyClassRef(PyClassName("java.lang.Throwable")))}(self):
       |        return self._scpy_cause
       |    def ${m("getTargetException")(PyClassRef(PyClassName("java.lang.Throwable")))}(self):
       |        return self._scpy_cause
       |class Spliterator(_scpy_Object): pass
       |class Reference(_scpy_Object):
       |    def __init__(self, referent=None, *_args):
       |        self._scpy_ref_referent = referent
       |    def ${m("get")(ObjRef)}(self):
       |        return self._scpy_ref_referent
       |    def ${m("clear")(V)}(self):
       |        self._scpy_ref_referent = None
       |class WeakReference(Reference): pass
       |class PrimitiveIterator(_scpy_Object): pass
       |class PrimitiveIterator_OfInt(PrimitiveIterator): pass
       |class PrimitiveIterator_OfLong(PrimitiveIterator): pass
       |class PrimitiveIterator_OfDouble(PrimitiveIterator): pass
       |
       |class Enum(Comparable, Serializable):
       |    def __init__(self, name, ordinal):
       |        self._scpy_enum_name = name
       |        self._scpy_enum_ordinal = ordinal
       |
       |    def ${m("name")(StrRef)}(self):
       |        return self._scpy_enum_name
       |
       |    def ${m("ordinal")(I)}(self):
       |        return self._scpy_enum_ordinal
       |
       |    def ${m("toString")(StrRef)}(self):
       |        return self._scpy_enum_name
       |
       |    def ${m("compareTo", PyClassRef(PyClassName("java.lang.Enum")))(I)}(self, other):
       |        return self._scpy_enum_ordinal - other.${m("ordinal")(I)}()
       |
       |    def ${m("compareTo", ObjRef)(I)}(self, other):
       |        return self.${m("compareTo", PyClassRef(PyClassName("java.lang.Enum")))(I)}(other)
       |
       |    def ${m("clone")(ObjRef)}(self):
       |        raise CloneNotSupportedException("Enums are not cloneable", None)
       |
       |    def ${m("finalize")(V)}(self):
       |        return None
       |
       |    def __str__(self):
       |        return self._scpy_enum_name
       |
       |class ClassLoader(_scpy_Object):
       |    def __init__(self, parent=None):
       |        self._scpy_parent = parent
       |
       |    def ${m("getParent")(PyClassRef(PyClassName("java.lang.ClassLoader")))}(self):
       |        return self._scpy_parent
       |
       |    def ${m("loadClass", StrRef)(ClsRef)}(self, name):
       |        # Walk the runtime class registry. If the class isn't
       |        # registered we mirror `Class.forName`'s behavior.
       |        clazz = _scpy_class_registry.get(name)
       |        if clazz is not None:
       |            return clazz
       |        return _scpy_ClassModule._scpy_resolve(name)
       |
       |    def ${m("loadClass", StrRef, Z)(ClsRef)}(self, name, resolve):
       |        return self.${m("loadClass", StrRef)(ClsRef)}(name)
       |
       |    def __getattr__(self, name):
       |        if name.startswith("loadClass"):
       |            return self.${m("loadClass", StrRef)(ClsRef)}
       |        if name.startswith("getResourceAsStream"):
       |            return lambda *args, **kw: None
       |        if name.startswith("getResource"):
       |            return lambda *args, **kw: None
       |        if name.startswith("getParent"):
       |            return self.${m("getParent")(PyClassRef(PyClassName("java.lang.ClassLoader")))}
       |        raise AttributeError(name)
       |
       |class ClassValue(_scpy_Object):
       |    def __init__(self):
       |        self._scpy_values = {}
       |
       |    def ${m("computeValue", ClsRef)(ObjRef)}(self, clazz):
       |        return None
       |
       |    def ${m("get", ClsRef)(ObjRef)}(self, clazz):
       |        if clazz is None:
       |            raise NullPointerException()
       |        if clazz not in self._scpy_values:
       |            self._scpy_values[clazz] = self.${m("computeValue", ClsRef)(ObjRef)}(clazz)
       |        return self._scpy_values[clazz]
       |
       |    def ${m("remove", ClsRef)(V)}(self, clazz):
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
       |    def ${m("toString")(StrRef)}(self):
       |        # JVM `BoxedUnit.toString()` returns `"()"` (see
       |        # `library/src/scala/runtime/BoxedUnit.java`). Without an
       |        # explicit override, `_scpy_to_str` would walk up to
       |        # `_scpy_Object.${m("toString")(StrRef)}` and produce
       |        # `scala.runtime.BoxedUnit@0` — observed in `tests/run/t5680.scala`
       |        # after fixing the `_scpy_Array.toString` cluster.
       |        return "()"
       |
       |# Stdlib accesses `BoxedUnit.UNIT` as a static field via
       |# `LoadModule(BoxedUnit) + Select(UNIT)`. Bind the static field
       |# on the class itself, then expose the same instance under the
       |# emitter's `_scpy_mod_*_` naming convention so all callers
       |# (LoadModule, ApplyStatic) hit it.
       |BoxedUnit.UNIT = BoxedUnit()
       |BoxedUnit.TYPE = _scpy_primitive_void
       |${mod(PyClassName("scala.runtime.BoxedUnit"))} = BoxedUnit
       |${mod(PyClassName("scala.runtime.BoxedUnit$"))} = BoxedUnit
       |
       |class _scpy_Char(int):
       |    # Boxed Char wrapper. Subclasses Python's `int` so existing
       |    # primitive Char operations (treated as ints by the backend)
       |    # still work — e.g. `_scpy_Char(97) - ord('0')` is an int op,
       |    # `hash` returns the codepoint, `<`/`>` use int ordering.
       |    #
       |    # The wrapper is what Scala emits when a `Char` is widened to
       |    # `Any`/`Object` (via `Char$$.box` or `BoxesRunTime.boxToCharacter`).
       |    # Without it, `_scpy_to_str(120)` would render as `"120"` instead
       |    # of `"x"`. The `${m("toString")(StrRef)}` method below is the
       |    # hook that `_scpy_to_str` discovers via `getattr`.
       |    __slots__ = ()
       |
       |    def __new__(cls, value):
       |        return int.__new__(cls, int(value))
       |
       |    def __str__(self):
       |        return _builtins.chr(int(self))
       |
       |    def __repr__(self):
       |        return _builtins.chr(int(self))
       |
       |    def __eq__(self, other):
       |        # Compare by codepoint against another `_scpy_Char` or a
       |        # raw int. The lenient int-friendly comparison is needed
       |        # because primitive Char paths in stdlib generic code
       |        # (e.g. `ArrayOps.ArrayIterator.next()` reading from a
       |        # primitive `Array[Char]`) return raw ints — never boxed —
       |        # so `arr.contains('a')` on a Char vector would otherwise
       |        # miss its own elements. Booleans are excluded because
       |        # Python's `True == 1` would otherwise be transitive.
       |        if isinstance(other, bool):
       |            return False
       |        if isinstance(other, int):
       |            return int(self) == int(other)
       |        return False
       |
       |    def __ne__(self, other):
       |        return not self.__eq__(other)
       |
       |    def __hash__(self):
       |        # Match `java.lang.Character.hashCode()`: the codepoint.
       |        return int(self)
       |
       |    def ${m("toString")(StrRef)}(self):
       |        return _builtins.chr(int(self))
       |
       |    def ${m("hashCode")(I)}(self):
       |        return int(self)
       |
       |    def ${m("charValue")(C)}(self):
       |        return int(self)
       |
       |    def ${m("equals", ObjRef)(Z)}(self, other):
       |        return isinstance(other, _scpy_Char) and int(other) == int(self)
       |
       |    def ${m("compareTo", PyClassRef(PyClassName("java.lang.Character")))(I)}(self, other):
       |        return int(self) - int(other)
       |
       |    def ${m("compareTo", ObjRef)(I)}(self, other):
       |        return int(self) - int(other)
       |
       |def _scpy_box_char(value):
       |    # Idempotent boxing: a value that is already a `_scpy_Char`
       |    # (e.g. when chained boxes are inserted by erasure) is left
       |    # unchanged so identity (`is`) is preserved where possible.
       |    if isinstance(value, _scpy_Char):
       |        return value
       |    return _scpy_Char(value)
       |
       |def _scpy_unbox_char(value):
       |    if value is None:
       |        raise NullPointerException()
       |    return int(value)
       |
       |# `scala.Int_` and `scala.Char_` companion module classes (and their
       |# `int2double` / `int2long` / `int2float` / `char2int` / `char2long`
       |# / `char2float` / `char2double` implicit-coercion methods) are
       |# supplied by the compiled `library-py` PyIR — they flow through the
       |# linker like every other `scala.Long_` / `scala.Float_` / etc.
       |# Earlier hand-written `_scpy_IntModule` / `_scpy_CharModule` stubs
       |# shadowed those compiled defs, so calls such as
       |# `${mod(PyClassName("scala.Int$"))}.${m("int2double", I)(D)}(i)` raised AttributeError.
       |# `Class.forName(...)` is a static call on the JDK-provided
       |# `java.lang.Class`. The backend lowers it to
       |# `${mod(PyClassName("java.lang.Class"))}.forName__...(...)`. Bind the module
       |# variable to a singleton of `_scpy_ClassModule` so the call
       |# resolves; cover the `Class$$` companion form too in case any
       |# emitted code carries the trailing-dollar variant.
       |${mod(PyClassName("java.lang.Class"))}  = _scpy_ClassModule()
       |${mod(PyClassName("java.lang.Class$"))} = ${mod(PyClassName("java.lang.Class"))}
       |_scpy_system_class_loader = ClassLoader()
       |""".stripMargin +
    raw"""|_scpy_register_class(object, "java.lang.Object", "class", None)
       |_scpy_register_class(str, "java.lang.String", "class", "java.lang.Object", ("java.lang.CharSequence", "java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(_scpy_Class, "java.lang.Class", "class", "java.lang.Object")
       |_scpy_register_class(ClassLoader, "java.lang.ClassLoader", "class", "java.lang.Object")
       |_scpy_register_class(ClassValue, "java.lang.ClassValue", "class", "java.lang.Object")
       |_scpy_register_class(VarHandle, "java.lang.invoke.VarHandle", "class", "java.lang.Object")
       |_scpy_register_class(MethodHandles, "java.lang.invoke.MethodHandles", "class", "java.lang.Object")
       |_scpy_register_class(MethodHandles_Lookup, "java.lang.invoke.MethodHandles_Lookup", "class", "java.lang.Object")
       |_scpy_register_class(AbstractStringBuilder, "java.lang.AbstractStringBuilder", "class", "java.lang.Object")
       |_scpy_register_class(AccessibleObject, "java.lang.reflect.AccessibleObject", "class", "java.lang.Object")
       |_scpy_register_class(Method, "java.lang.reflect.Method", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Field, "java.lang.reflect.Field", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Executable, "java.lang.reflect.Executable", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Constructor, "java.lang.reflect.Constructor", "class", "java.lang.reflect.AccessibleObject")
       |_scpy_register_class(Type, "java.lang.reflect.Type", "interface", None)
       |_scpy_register_class(TypeVariable, "java.lang.reflect.TypeVariable", "interface", None, ("java.lang.reflect.Type",))
       |_scpy_register_class(Modifier, "java.lang.reflect.Modifier", "class", "java.lang.Object")
       |_scpy_register_class(InvocationTargetException, "java.lang.reflect.InvocationTargetException", "class", "java.lang.Object")
       |_scpy_register_class(Spliterator, "java.util.Spliterator", "interface", None)
       |_scpy_register_class(Reference, "java.lang.ref.Reference", "class", "java.lang.Object")
       |_scpy_register_class(WeakReference, "java.lang.ref.WeakReference", "class", "java.lang.ref.Reference")
       |_scpy_register_class(PrimitiveIterator, "java.util.PrimitiveIterator", "interface", None)
       |_scpy_register_class(PrimitiveIterator_OfInt, "java.util.PrimitiveIterator_OfInt", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(PrimitiveIterator_OfLong, "java.util.PrimitiveIterator_OfLong", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(PrimitiveIterator_OfDouble, "java.util.PrimitiveIterator_OfDouble", "interface", None, ("java.util.PrimitiveIterator",))
       |_scpy_register_class(Comparable, "java.lang.Comparable", "interface", None)
       |_scpy_register_class(Serializable, "java.io.Serializable", "interface", None)
       |_scpy_register_class(Enum, "java.lang.Enum", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(BoxedUnit, "scala.runtime.BoxedUnit", "class", "java.lang.Object")
       |_scpy_register_class(None, "java.lang.Cloneable", "interface", None)
       |_scpy_register_class(None, "java.lang.Number", "class", "java.lang.Object", ("java.io.Serializable",))
       |_scpy_register_class(None, "java.lang.Boolean", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(_scpy_Char, "java.lang.Character", "class", "java.lang.Object", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Byte", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Short", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Integer", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Long", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Float", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |_scpy_register_class(None, "java.lang.Double", "class", "java.lang.Number", ("java.lang.Comparable", "java.io.Serializable"))
       |# Scala tuple classes — runtime-provided, backed by native Python
       |# tuples. No `py_type` is bound: the runtime never instantiates
       |# these classes, only consults `_scpy_class_of_name(...)` for
       |# `isInstanceOf[Tuple{N}]` checks (special-cased in
       |# `_scpy_is_value_of_type`).
       |_scpy_register_class(None, "scala.Tuple", "interface", None)
       |_scpy_register_class(None, "scala.NonEmptyTuple", "interface", None, ("scala.Tuple",))
       |_scpy_register_class(None, "scala._times_colon", "class", "java.lang.Object", ("scala.NonEmptyTuple",))
       |_scpy_register_class(None, "scala.EmptyTuple_", "class", "java.lang.Object", ("scala.Tuple",))
       |_scpy_register_class(None, "scala.runtime.TupleXXL", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.runtime.Tuples_", "class", "java.lang.Object")
       |_scpy_register_class(None, "scala.Tuple1", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple2", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple3", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple4", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple5", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple6", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple7", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple8", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple9", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple10", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple11", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple12", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple13", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple14", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple15", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple16", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple17", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple18", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple19", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple20", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple21", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
       |_scpy_register_class(None, "scala.Tuple22", "class", "java.lang.Object", ("scala._times_colon", "scala.NonEmptyTuple"))
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
       |            cls = object.__getattribute__(self, "_scpy_cls")
       |            # Two-step init: first the synthesized no-arg
       |            # `__init__` (JVM-style field zero-init) for every
       |            # class in the MRO, then the encoded no-arg ctor body
       |            # that actually runs `<init>` for the module. We must
       |            # walk the MRO root-first (mirroring `_scpy_new`)
       |            # because each class's `__init__` only sets its OWN
       |            # fields — Python doesn't auto-chain to parent
       |            # `__init__`. Without this, an inherited `var x: T`
       |            # without an explicit initializer (e.g.
       |            # `scala.Enumeration.nextName`) reads back as a
       |            # missing attribute on the subclass instance. Module
       |            # classes always have a no-arg ctor; for the rare
       |            # case where it has been DCE'd (no Scala-side
       |            # reference), fall back to whatever ctor helper
       |            # survives, alphabetically first.
       |            for _scpy_klass in reversed(cls.__mro__):
       |                if "_scpy_full_name" not in _scpy_klass.__dict__:
       |                    continue
       |                _scpy_init = _scpy_klass.__dict__.get("__init__")
       |                if _scpy_init is not None:
       |                    _scpy_init(inst)
       |            no_arg_helper = "_scpy_ctor_" + cls.__name__ + "__${m("void")(V)}"
       |            ctor = getattr(cls, no_arg_helper, None)
       |            if ctor is not None:
       |                ctor(inst)
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
       |# Constructor allocation + dispatch helper.
       |#
       |# `PyNew(Cls, ctor, args)` lowers at codegen time to
       |#   `_scpy_new(Cls, Cls._scpy_ctor_<encoded-sig>, args...)`
       |# so that the chosen constructor overload is fixed by symbol
       |# identity in Scala source — no runtime arity / type-guard
       |# dispatch. The flow is:
       |#
       |#   1. `cls.__new__(cls)` allocates a blank instance.
       |#   2. The class's no-arg `__init__(self)` runs to perform JVM-
       |#      style field zero-init (`null`/`0`/`false` defaults that
       |#      the ctor body itself may not re-assign — e.g. `var x: T`
       |#      with no explicit initializer would otherwise raise
       |#      AttributeError on first read).
       |#   3. The selected ctor helper runs the actual Scala `<init>`
       |#      body on the now-zero-initialized instance, including any
       |#      `super.<init>(...)` chain and `this.x = ...` assignments
       |#      that the user wrote.
       |#
       |# Step 2 is required because Python attribute access raises rather
       |# than returning a sentinel for missing attributes; Scala source
       |# relies on JVM "every field starts as zero/null" semantics.
       |def _scpy_new(cls, ctor, *args):
       |    obj = cls.__new__(cls)
       |    # Walk MRO root-first so each class's own zero-init runs. The
       |    # generated `__init__(*args)` short-circuits on no args and
       |    # just executes its per-class zero-init prefix (assigning
       |    # JVM-default values to that class's own fields/lazy holders).
       |    # We must visit every class in the chain because a subclass
       |    # `__init__` only sets its OWN fields — Python doesn't auto-
       |    # chain to parent `__init__`. Python built-in ancestors
       |    # (`object`, `Exception`, …) are skipped via the
       |    # `_scpy_full_name` marker; calling their `__init__` could
       |    # set unwanted attributes (e.g. `Exception.args`).
       |    for _scpy_klass in reversed(cls.__mro__):
       |        if "_scpy_full_name" not in _scpy_klass.__dict__:
       |            continue
       |        _scpy_init = _scpy_klass.__dict__.get("__init__")
       |        if _scpy_init is not None:
       |            _scpy_init(obj)
       |    ctor(obj, *args)
       |    return obj
       |
       |# Closure carrier base. Lambdas emitted from Scala source reach
       |# here via `_scpy_FnN(lambda ...)` for `N = 0..22`. The arity-
       |# specific `_scpy_FnN` subclasses (which also extend the nominal
       |# `scala.FunctionN`) are emitted by `PyIREmitter` AFTER class
       |# registrations finish, so the parent identifier resolves
       |# correctly whether the FunctionN nominal stub lives in the
       |# runtime prelude (simple name) or in a downstream `.pyir`
       |# (mangled FQN) — `PyIRRuntime.classIdentifier` flips the shape
       |# based on `providedClasses`.
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
       |# Translate native Python arithmetic errors into JDK exception types so
       |# they are admissible as `java.lang.Throwable` values for `scala.util.Try`,
       |# `try/catch (e: ArithmeticException)`, etc. Keeps the Scala-overflow
       |# wrapping (`_scpy_i32` / `_scpy_i64`) on the success path.
       |def _scpy_idiv(a, b):
       |    try:
       |        return _scpy_i32(a // b)
       |    except ZeroDivisionError:
       |        raise ArithmeticException("/ by zero")
       |
       |def _scpy_imod(a, b):
       |    try:
       |        return _scpy_i32(a % b)
       |    except ZeroDivisionError:
       |        raise ArithmeticException("/ by zero")
       |
       |def _scpy_ldiv(a, b):
       |    try:
       |        return _scpy_i64(a // b)
       |    except ZeroDivisionError:
       |        raise ArithmeticException("/ by zero")
       |
       |def _scpy_lmod(a, b):
       |    try:
       |        return _scpy_i64(a % b)
       |    except ZeroDivisionError:
       |        raise ArithmeticException("/ by zero")
       |
       |def _scpy_float_to_str(x):
       |    # Format a finite Double the way `java.lang.Double.toString`
       |    # does on top of Python's `repr(float)`. The two algorithms
       |    # agree on shortest-roundtrip significand digits in most
       |    # cases (a handful of subnormal / nearly-half-ULP corners
       |    # round differently between Python's algorithm and Java's
       |    # Ryu — those will still diverge), but they disagree on the
       |    # surface format:
       |    #   * exponent thresholds — Java uses scientific for
       |    #     |x| < 1e-3 OR |x| >= 1e7; Python uses 1e-4 / 1e16.
       |    #   * exponent letter case and zero-padding — Python emits
       |    #     `1e+20` / `1e-09`; Java emits `1.0E20` / `1.0E-9`.
       |    #   * integer-valued floats — Java always has a trailing
       |    #     `.0` (`1.0`), Python's `repr` does too in most cases.
       |    # Negative zero is `-0.0` in both. Sign on `+` exponents is
       |    # absent on the Java side.
       |    if _scpy_math.isnan(x):
       |        return "NaN"
       |    if _scpy_math.isinf(x):
       |        return "Infinity" if x > 0 else "-Infinity"
       |    if x == 0.0:
       |        return "-0.0" if _scpy_math.copysign(1.0, x) < 0 else "0.0"
       |    abs_x = -x if x < 0 else x
       |    # Java's scientific-notation cutoffs.
       |    use_sci = abs_x < 1e-3 or abs_x >= 1e7
       |    s = _builtins.repr(x)
       |    e_idx = s.find("e")
       |    if e_idx < 0:
       |        e_idx = s.find("E")
       |    has_exp = e_idx >= 0
       |    if use_sci and not has_exp:
       |        # Coerce plain-form input (`0.0001`) to scientific. Use
       |        # `{:e}` for shortest mantissa; trailing zeros are
       |        # stripped below.
       |        s = "{:e}".format(x)
       |        e_idx = s.find("e")
       |    elif not use_sci and has_exp:
       |        # Coerce scientific-form input (`1e+16`) to plain.
       |        s = "{:.20f}".format(x).rstrip("0")
       |        if s.endswith("."):
       |            s = s + "0"
       |        e_idx = -1
       |    if e_idx >= 0:
       |        mantissa = s[:e_idx]
       |        exp_str = s[e_idx + 1:]
       |        if exp_str.startswith("+"):
       |            exp_str = exp_str[1:]
       |        neg_exp = exp_str.startswith("-")
       |        if neg_exp:
       |            exp_str = exp_str[1:]
       |        exp_str = exp_str.lstrip("0") or "0"
       |        if neg_exp:
       |            exp_str = "-" + exp_str
       |        if "." not in mantissa:
       |            mantissa = mantissa + ".0"
       |        else:
       |            int_part, frac_part = mantissa.split(".", 1)
       |            stripped = frac_part.rstrip("0")
       |            mantissa = int_part + "." + (stripped if stripped else "0")
       |        return mantissa + "E" + exp_str
       |    if "." not in s:
       |        s = s + ".0"
       |    return s
       |
       |def _scpy_to_str(x):
       |    # Scala-faithful stringification: matches `String.valueOf`
       |    if x is None:
       |        return "null"
       |    if x is True:
       |        return "true"
       |    if x is False:
       |        return "false"
       |    # Scala tuples lower to native Python tuples but are tagged with
       |    # the `_scpy_ScalaTuple` subclass; format only those Scala-style
       |    # so foreign tuples returned by `@extern` Python facades keep
       |    # Python's native repr (e.g. `np.shape == (2, 3)`).
       |    if isinstance(x, _scpy_ScalaTuple):
       |        return _scpy_tuple_to_str(x)
       |    to_string = getattr(x, "${m("toString")(StrRef)}", None)
       |    if to_string is not None:
       |        return to_string()
       |    # Boxed Doubles arrive here as plain Python floats. Java's
       |    # `Double.toString` differs from Python's `repr` on exponent
       |    # thresholds, exponent capitalisation, and integer-valued
       |    # `.0` suffixes; route them through the shared formatter so
       |    # `println(d)` matches `String.valueOf(d)`.
       |    if isinstance(x, _builtins.float) and not isinstance(x, _builtins.bool):
       |        return _scpy_float_to_str(x)
       |    return _builtins.str(x)
       |
       |def _scpy_call_to_string(x):
       |    if x is None:
       |        raise NullPointerException()
       |    return _scpy_to_str(x)
       |
       |# Lambda parameter unboxing for primitive-typed SAM call sites.
       |# A specialized closure like `Int => Int` may still be invoked through
       |# the boxed `Function1.apply(Object): Object` SAM bridge with a
       |# `null`/`None` argument (e.g. `genericCall1` calls
       |# `foo(null.asInstanceOf[A])`). On the JVM the specialization bridge
       |# unboxes `null -> 0`/`0L`/`false` before forwarding to the primitive
       |# body. `GenPython.genClosure` injects this helper around each SAM
       |# argument whose target parameter is primitive; non-primitive SAM
       |# parameters bypass the wrapper entirely.
       |def _scpy_unbox_or_default(tag, value):
       |    if value is not None:
       |        return value
       |    if tag == "I" or tag == "S" or tag == "B" or tag == "C":
       |        return 0
       |    if tag == "J":
       |        return 0
       |    if tag == "F" or tag == "D":
       |        return 0.0
       |    if tag == "Z":
       |        return False
       |    return None
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
       |        raise ArithmeticException("/ by zero")
       |    quot = abs(a) // abs(b)
       |    return -quot if (a < 0) ^ (b < 0) else quot
       |
       |def _scpy_int_trunc_mod(a, b):
       |    # Java/Scala-style integer modulo: the result has the sign of
       |    # the dividend, matching `a - (a / b) * b` under truncation
       |    # toward zero. Python's `%` is floor-modulo (sign of divisor),
       |    # so we adjust manually for negative operands.
       |    if b == 0:
       |        raise ArithmeticException("/ by zero")
       |    rem = abs(a) % abs(b)
       |    return -rem if a < 0 else rem
       |
       |def _scpy_int_udiv32(a, b):
       |    if b == 0:
       |        raise ArithmeticException("/ by zero")
       |    return ((a & 0xFFFFFFFF) // (b & 0xFFFFFFFF)) & 0xFFFFFFFF
       |
       |def _scpy_int_urem32(a, b):
       |    if b == 0:
       |        raise ArithmeticException("/ by zero")
       |    return ((a & 0xFFFFFFFF) % (b & 0xFFFFFFFF)) & 0xFFFFFFFF
       |
       |def _scpy_int_udiv64(a, b):
       |    if b == 0:
       |        raise ArithmeticException("/ by zero")
       |    return ((a & 0xFFFFFFFFFFFFFFFF) // (b & 0xFFFFFFFFFFFFFFFF)) & 0xFFFFFFFFFFFFFFFF
       |
       |def _scpy_int_urem64(a, b):
       |    if b == 0:
       |        raise ArithmeticException("/ by zero")
       |    return ((a & 0xFFFFFFFFFFFFFFFF) % (b & 0xFFFFFFFFFFFFFFFF)) & 0xFFFFFFFFFFFFFFFF
       |
       |def _scpy_int_ushr32(value, shift):
       |    # Java `>>>` semantics on a 32-bit operand: treat `value` as
       |    # an unsigned 32-bit int, then logical shift right.
       |    return ((value & 0xFFFFFFFF) >> (shift & 31)) & 0xFFFFFFFF
       |
       |def _scpy_int_ushr64(value, shift):
       |    return ((value & 0xFFFFFFFFFFFFFFFF) >> (shift & 63)) & 0xFFFFFFFFFFFFFFFF
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
       |def _scpy_any_hash_code(x):
       |    # Models virtual `Object.hashCode()` dispatch for receivers
       |    # whose static type is `Any`/`Object`/primitive-boxed. Unlike
       |    # `Statics.anyHash` (the `.##` lowering, which applies JVM's
       |    # cross-numeric collapse rule so `5L.## == 5.##`), this matches
       |    # the boxed primitives' own `hashCode()` methods, which never
       |    # collapse:
       |    #   * `Boolean.hashCode = 1231/1237`
       |    #   * `Integer.hashCode = intValue` (signed-32 truncation)
       |    #   * `Long.hashCode    = (int)lv ^ (int)(lv >>> 32)`
       |    #   * `Double.hashCode  = Long.hashCode(doubleToLongBits)`
       |    # The Float-vs-Double distinction is unrecoverable from a raw
       |    # Python `float`, so we bias toward Double; the Float boxed
       |    # case (`Float.hashCode = floatToIntBits`) is not reachable
       |    # without a runtime boxed-Float wrapper, and the
       |    # `tests/run/hashhash.scala` Float-typed `.##` vs `.hashCode`
       |    # gap is a known unsupportable case for this backend.
       |    if x is None:
       |        return 0
       |    if isinstance(x, bool):
       |        return 1231 if x else 1237
       |    if isinstance(x, int):
       |        # Apply the boxed-Long pattern (Int values fall out of the
       |        # in-range short-circuit; out-of-range values use the
       |        # `(int)lv ^ (int)(lv >>> 32)` mix). CPython `hash(int)`
       |        # collapses `-1 -> -2` and reduces large ints mod
       |        # `sys.hash_info.modulus`, so we can't defer to it.
       |        if -0x80000000 <= x <= 0x7FFFFFFF:
       |            return x
       |        masked = x & 0xFFFFFFFFFFFFFFFF
       |        lo = masked & 0xFFFFFFFF
       |        hi = (masked >> 32) & 0xFFFFFFFF
       |        result = (lo ^ hi) & 0xFFFFFFFF
       |        return result - 0x100000000 if result >= 0x80000000 else result
       |    if isinstance(x, float):
       |        # The Float-vs-Double distinction is unrecoverable from a
       |        # raw Python `float` (both Scala types lower to it). When
       |        # the value round-trips through 32-bit float, prefer the
       |        # `Float.hashCode = floatToIntBits` algorithm — this is
       |        # what `tests/run/hashhash.scala` `confirmSame(5.5f)`
       |        # demands (its JVM-side .## also routes through
       |        # `Float.hashCode` via the `Statics.doubleHash` collapse
       |        # chain when iv/lv don't round-trip). For non-Float-
       |        # representable doubles, fall through to
       |        # `Double.hashCode = Long.hashCode(doubleToLongBits)`.
       |        if _scpy_math.isnan(x):
       |            return 2146959360  # Long.hashCode of canonical Double NaN
       |        try:
       |            fv_bits = struct.unpack('<i', struct.pack('<f', x))[0]
       |            fv_round = struct.unpack('<f', struct.pack('<f', x))[0]
       |            if float(fv_round) == x:
       |                return fv_bits
       |        except (OverflowError, ValueError):
       |            pass
       |        bits = struct.unpack('<q', struct.pack('<d', x))[0]
       |        masked = bits & 0xFFFFFFFFFFFFFFFF
       |        lo = masked & 0xFFFFFFFF
       |        hi = (masked >> 32) & 0xFFFFFFFF
       |        result = (lo ^ hi) & 0xFFFFFFFF
       |        return result - 0x100000000 if result >= 0x80000000 else result
       |    if isinstance(x, str):
       |        return _scpy_str_hash_code(x)
       |    # Scala tuples (Python tuples tagged with `_scpy_ScalaTuple`)
       |    # use JVM-bit-exact `MurmurHash3.caseClassHash` so
       |    # `(1, 2).hashCode` matches the JVM. Foreign Python tuples
       |    # from `@extern` facades fall through to Python's native
       |    # `tuple.__hash__`.
       |    if isinstance(x, _scpy_ScalaTuple):
       |        return _scpy_tuple_hash(x)
       |    # Non-primitive: defer to the Scala/Python `__hash__` slot.
       |    return x.__hash__()
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
       |# Polymorphic helpers for `java.lang.CharSequence` interface methods.
       |# A `CharSequence`-typed receiver may carry a Python `str` at runtime
       |# (no encoded `subSequence__I_I__...` etc.) or a real ported subclass
       |# like `StringBuilder`/`ArrayCharSequence` that implements the
       |# encoded methods. Dispatch on `isinstance(s, str)` so both shapes
       |# work.
       |def _scpy_charseq_length(s):
       |    if isinstance(s, str):
       |        return len(s)
       |    return s.${m("length")(I)}()
       |
       |def _scpy_charseq_char_at(s, index):
       |    if isinstance(s, str):
       |        return _scpy_str_char_at(s, index)
       |    return s.${m("charAt", I)(C)}(index)
       |
       |def _scpy_charseq_sub_sequence(s, begin, end):
       |    if isinstance(s, str):
       |        return _scpy_str_substring(s, begin, end)
       |    return s.${m("subSequence", I, I)(PyClassRef(PyClassName("java.lang.CharSequence")))}(begin, end)
       |
       |def _scpy_charseq_is_empty(s):
       |    if isinstance(s, str):
       |        return len(s) == 0
       |    return s.${m("isEmpty")(Z)}()
       |
       |def _scpy_charseq_to_string(s):
       |    if isinstance(s, str):
       |        return s
       |    return s.${m("toString")(StrRef)}()
       |
       |# Polymorphic helpers for `java.lang.Double`/`Float`/`Long`/`Integer`/
       |# `Boolean` instance methods invoked through an `Object`/`AnyRef`/
       |# `java.lang.Number`/boxed-primitive static receiver. The runtime
       |# value may be a raw Python `float`/`int`/`bool` (boxing is identity
       |# on this backend — `boxToDouble` returns the same `float`), in which
       |# case the encoded method name (`${m("isNaN")(Z)}`, `${m("intValue")(I)}`, ...) does
       |# not resolve. Dispatch on `isinstance(...)` so calls on raw
       |# primitives produce JVM-faithful values, and fall through to the
       |# encoded method on real ported boxed instances (e.g. one minted
       |# explicitly via `new java.lang.Double(d)` — see Wave 5 item 05's
       |# `explicit-boxed-number-not-stored` discussion).
       |#
       |# `bool` is a subclass of `int` in Python, so the bool checks must
       |# come first wherever a primitive int receiver would also accept a
       |# bool (currently none — Boolean methods are gated on bool only).
       |def _scpy_Double_isNaN(x):
       |    if isinstance(x, bool):
       |        return False
       |    if isinstance(x, (int, float)):
       |        return _scpy_math.isnan(x) if isinstance(x, float) else False
       |    return x.${m("isNaN")(Z)}()
       |
       |def _scpy_Double_isInfinite(x):
       |    if isinstance(x, bool):
       |        return False
       |    if isinstance(x, (int, float)):
       |        return _scpy_math.isinf(x) if isinstance(x, float) else False
       |    return x.${m("isInfinite")(Z)}()
       |
       |def _scpy_Double_doubleValue(x):
       |    if isinstance(x, bool):
       |        return 1.0 if x else 0.0
       |    if isinstance(x, (int, float)):
       |        return _builtins.float(x)
       |    return x.${m("doubleValue")(D)}()
       |
       |def _scpy_Double_floatValue(x):
       |    if isinstance(x, bool):
       |        return 1.0 if x else 0.0
       |    if isinstance(x, (int, float)):
       |        return _scpy_f32(x)
       |    return x.${m("floatValue")(F)}()
       |
       |def _scpy_Double_intValue(x):
       |    if isinstance(x, bool):
       |        return 1 if x else 0
       |    if isinstance(x, float):
       |        # `Double.intValue() = (int)d`: truncate toward zero, then
       |        # apply JVM 32-bit narrowing (`d2i` on infinity / NaN /
       |        # out-of-range values clamps to Int.{MaxValue, MinValue, 0}).
       |        if _scpy_math.isnan(x):
       |            return 0
       |        if x >= 2147483647.0:
       |            return 2147483647
       |        if x <= -2147483648.0:
       |            return -2147483648
       |        return _builtins.int(x)
       |    if isinstance(x, int):
       |        return _scpy_i32(x)
       |    return x.${m("intValue")(I)}()
       |
       |def _scpy_Double_longValue(x):
       |    if isinstance(x, bool):
       |        return 1 if x else 0
       |    if isinstance(x, float):
       |        if _scpy_math.isnan(x):
       |            return 0
       |        if x >= 9223372036854775807.0:
       |            return 9223372036854775807
       |        if x <= -9223372036854775808.0:
       |            return -9223372036854775808
       |        return _scpy_i64(_builtins.int(x))
       |    if isinstance(x, int):
       |        return _scpy_i64(x)
       |    return x.${m("longValue")(J)}()
       |
       |def _scpy_Double_byteValue(x):
       |    # `Number.byteValue() = (byte)intValue()`: signed-8-bit narrow.
       |    v = _scpy_Double_intValue(x) & 0xFF
       |    return v - 0x100 if v >= 0x80 else v
       |
       |def _scpy_Double_shortValue(x):
       |    # `Number.shortValue() = (short)intValue()`: signed-16-bit narrow.
       |    v = _scpy_Double_intValue(x) & 0xFFFF
       |    return v - 0x10000 if v >= 0x8000 else v
       |
       |def _scpy_Boolean_booleanValue(x):
       |    if isinstance(x, bool):
       |        return x
       |    return x.${m("booleanValue")(Z)}()
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
       |            hasattr(encoding[0], '${m("encode", StrRef)(PyClassRef(PyClassName("java.nio.ByteBuffer")))}')
       |    ):
       |        bb = encoding[0].${m("encode", StrRef)(PyClassRef(PyClassName("java.nio.ByteBuffer")))}(s)
       |        remaining = bb.${m("remaining")(I)}()
       |        out = _scpy_new_array(_scpy_primitive_byte, remaining, 0)
       |        if remaining > 0:
       |            bb.${m("get", PyArrayRef(B, 1))(PyClassRef(PyClassName("java.nio.ByteBuffer")))}(out)
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
       |        elif hasattr(candidate, '${m("name")(StrRef)}'):
       |            enc = candidate.${m("name")(StrRef)}()
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
       |""".stripMargin +
    raw"""|# -- Tuple helpers --
       |# Scala tuples lower to native Python tuples (heterogeneous,
       |# immutable, hashable). These helpers implement the surface of
       |# `scala.runtime.Tuples.*` plus the `Product` machinery the
       |# compiler routes here. Boundary handling for arity 22 ↔ 23
       |# disappears: a Python tuple has no upper bound.
       |#
       |# `_scpy_ScalaTuple` is a tag-only subclass of `tuple`. It
       |# distinguishes Scala-tuple values from foreign Python tuples
       |# returned by `@extern` facades (e.g. `np.shape`, `math.frexp`).
       |# The two kinds compare and hash equal under Python's content-
       |# based `tuple.__eq__`/`__hash__`, so interop equality still
       |# works; only string formatting and a few `isinstance` checks
       |# branch on the tag.
       |class _scpy_ScalaTuple(tuple):
       |    __slots__ = ()
       |    def __hash__(self):
       |        # JVM-bit-exact `MurmurHash3.caseClassHash` so a dict-key
       |        # `(1, 2)` constructed in Scala lookups identically to one
       |        # constructed via `*:` cons. `_scpy_tuple_hash` is forward-
       |        # declared lower in the prelude.
       |        return _scpy_tuple_hash(self)
       |
       |def _scpy_st(t):
       |    # Wrap an arbitrary tuple as a Scala tuple. Idempotent:
       |    # already-tagged inputs pass through.
       |    if t.__class__ is _scpy_ScalaTuple:
       |        return t
       |    return _scpy_ScalaTuple(t)
       |
       |def _scpy_tuple_get(t, i):
       |    if i < 0 or i >= len(t):
       |        raise IndexOutOfBoundsException(_builtins.str(i))
       |    return t[i]
       |
       |def _scpy_tuple_concat(a, b):
       |    return _scpy_ScalaTuple(a + b)
       |
       |def _scpy_tuple_cons(x, t):
       |    return _scpy_ScalaTuple((x,) + t)
       |
       |def _scpy_tuple_append(x, t):
       |    return _scpy_ScalaTuple(t + (x,))
       |
       |def _scpy_tuple_tail(t):
       |    if len(t) == 0:
       |        raise UnsupportedOperationException("tail of empty tuple", None)
       |    return _scpy_ScalaTuple(t[1:])
       |
       |def _scpy_tuple_init(t):
       |    if len(t) == 0:
       |        raise UnsupportedOperationException("init of empty tuple", None)
       |    return _scpy_ScalaTuple(t[:-1])
       |
       |def _scpy_tuple_last(t):
       |    if len(t) == 0:
       |        raise NoSuchElementException()
       |    return t[-1]
       |
       |def _scpy_tuple_size(t):
       |    return len(t)
       |
       |def _scpy_tuple_take(t, n):
       |    if n < 0:
       |        raise IndexOutOfBoundsException(_builtins.str(n))
       |    if n >= len(t):
       |        return _scpy_st(t)
       |    return _scpy_ScalaTuple(t[:n])
       |
       |def _scpy_tuple_drop(t, n):
       |    if n < 0:
       |        raise IndexOutOfBoundsException(_builtins.str(n))
       |    if n >= len(t):
       |        return _scpy_ScalaTuple()
       |    return _scpy_ScalaTuple(t[n:])
       |
       |def _scpy_tuple_splitat(t, n):
       |    if n < 0:
       |        raise IndexOutOfBoundsException(_builtins.str(n))
       |    if n >= len(t):
       |        return _scpy_ScalaTuple((_scpy_st(t), _scpy_ScalaTuple()))
       |    return _scpy_ScalaTuple((_scpy_ScalaTuple(t[:n]), _scpy_ScalaTuple(t[n:])))
       |
       |def _scpy_tuple_reverse(t):
       |    return _scpy_ScalaTuple(t[::-1])
       |
       |def _scpy_tuple_zip(a, b):
       |    n = len(a) if len(a) < len(b) else len(b)
       |    return _scpy_ScalaTuple(_scpy_ScalaTuple((a[i], b[i])) for i in range(n))
       |
       |def _scpy_tuple_map(t, f):
       |    return _scpy_ScalaTuple(f.${m("apply", ObjRef)(ObjRef)}(x) for x in t)
       |
       |def _scpy_tuple_prefix(t):
       |    n = len(t)
       |    if n == 0:
       |        return ""
       |    if n <= 22:
       |        return "Tuple" + _builtins.str(n)
       |    return "Tuple"
       |
       |def _scpy_tuple_element_name(t, n):
       |    return "_" + _builtins.str(n + 1)
       |
       |def _scpy_tuple_to_str(t):
       |    n = len(t)
       |    if n == 0:
       |        return "()"
       |    return "(" + ",".join(_scpy_to_str(x) for x in t) + ")"
       |
       |# JVM-bit-exact `MurmurHash3.caseClassHash` for tuples. Mirrors
       |# `scala.runtime.Statics.{mix,mixLast,finalizeHash,avalanche}` and
       |# the `caseClassHash` body in `scala.util.hashing.MurmurHash3`.
       |# Pinned tests in `scala-runtime-statics-hash.scala` rely on these
       |# values matching JVM `Tuple{N}.hashCode` exactly. Element hashes
       |# go through `_scpy_any_hash_code` so they match the JVM
       |# boxed-primitive `hashCode` rules (`Long.hashCode = (int)lv ^
       |# (int)(lv >>> 32)`, etc.).
       |def _scpy_tuple_mh3_rotl(x, n):
       |    u = x & 0xFFFFFFFF
       |    r = ((u << n) | (u >> (32 - n))) & 0xFFFFFFFF
       |    return r - 0x100000000 if r >= 0x80000000 else r
       |
       |def _scpy_tuple_mh3_mix_last(h, data):
       |    k = _scpy_i32(data * 0xcc9e2d51)
       |    k = _scpy_tuple_mh3_rotl(k, 15)
       |    k = _scpy_i32(k * 0x1b873593)
       |    return _scpy_i32(h ^ k)
       |
       |def _scpy_tuple_mh3_mix(h, data):
       |    h = _scpy_tuple_mh3_mix_last(h, data)
       |    h = _scpy_tuple_mh3_rotl(h, 13)
       |    return _scpy_i32(_scpy_i32(h * 5) + 0xe6546b64)
       |
       |def _scpy_tuple_mh3_avalanche(h):
       |    h = _scpy_i32(h ^ ((h & 0xFFFFFFFF) >> 16))
       |    h = _scpy_i32(h * 0x85ebca6b)
       |    h = _scpy_i32(h ^ ((h & 0xFFFFFFFF) >> 13))
       |    h = _scpy_i32(h * 0xc2b2ae35)
       |    h = _scpy_i32(h ^ ((h & 0xFFFFFFFF) >> 16))
       |    return h
       |
       |def _scpy_tuple_mh3_finalize(h, length):
       |    return _scpy_tuple_mh3_avalanche(_scpy_i32(h ^ length))
       |
       |def _scpy_tuple_hash(t):
       |    n = len(t)
       |    aye = _scpy_str_hash_code(_scpy_tuple_prefix(t))
       |    if n == 0:
       |        return aye
       |    # MurmurHash3.productSeed = 0xcafebabe → -889275714 signed
       |    h = -889275714
       |    h = _scpy_tuple_mh3_mix(h, aye)
       |    for i in range(n):
       |        h = _scpy_tuple_mh3_mix(h, _scpy_any_hash_code(t[i]))
       |    return _scpy_tuple_mh3_finalize(h, n)
       |
       |def _scpy_tuple_to_array(t):
       |    return _scpy_array_value(_scpy_class_of_name("java.lang.Object"), list(t))
       |
       |def _scpy_tuple_to_iarray(t):
       |    return _scpy_array_value(_scpy_class_of_name("java.lang.Object"), list(t))
       |
       |def _scpy_tuple_from_array(arr):
       |    if arr is None:
       |        return _scpy_ScalaTuple()
       |    return _scpy_ScalaTuple(arr)
       |
       |def _scpy_tuple_from_iarray(arr):
       |    if arr is None:
       |        return _scpy_ScalaTuple()
       |    return _scpy_ScalaTuple(arr)
       |
       |def _scpy_tuple_from_product(p):
       |    if p is None:
       |        return _scpy_ScalaTuple()
       |    if isinstance(p, tuple):
       |        return _scpy_st(p)
       |    n = p.${m("productArity")(I)}()
       |    return _scpy_ScalaTuple(p.${m("productElement", I)(ObjRef)}(i) for i in range(n))
       |
       |# Iterator over a tuple.
       |#
       |# `scala.collection.AbstractIterator` is defined in pylib (loaded
       |# AFTER this prelude), so the iterator class cannot extend it at
       |# definition time. Defer construction: on first call,
       |# `_scpy_tuple_iter` looks up `AbstractIterator` from the
       |# registered-class table and builds a subclass dynamically. All
       |# Iterator default methods (`toList`, `toSeq`, `mkString`,
       |# `foreach`, `map`, `filter`, …) are then inherited at runtime.
       |_scpy_TupleIterator = None
       |
       |def _scpy_tuple_iter(t):
       |    global _scpy_TupleIterator
       |    if _scpy_TupleIterator is None:
       |        abs_iter_cls = _scpy_class_registry.get("scala.collection.AbstractIterator")
       |        base_py = None
       |        if abs_iter_cls is not None:
       |            base_py = getattr(abs_iter_cls, "_scpy_py_type", None)
       |        if base_py is None:
       |            # AbstractIterator not yet registered (e.g. pylib not loaded
       |            # in this bundle). Fall back to a bare class — only
       |            # `hasNext`/`next`/`mkString` work in that mode.
       |            base_py = object
       |        def _iter_init(self, tup):
       |            self._scpy_t = tup
       |            self._scpy_i = 0
       |            try:
       |                base_py.__init__(self)
       |            except TypeError:
       |                pass
       |        def _iter_has_next(self):
       |            return self._scpy_i < len(self._scpy_t)
       |        def _iter_next(self):
       |            if self._scpy_i >= len(self._scpy_t):
       |                raise NoSuchElementException()
       |            v = self._scpy_t[self._scpy_i]
       |            self._scpy_i += 1
       |            return v
       |        body = {
       |            "__init__": _iter_init,
       |            "${m("hasNext")(Z)}": _iter_has_next,
       |            "${m("next")(ObjRef)}": _iter_next,
       |        }
       |        _scpy_TupleIterator = type("_scpy_TupleIterator", (base_py,), body)
       |    return _scpy_TupleIterator(t)
       |
       |def _scpy_isinstance_tuple(x):
       |    return isinstance(x, tuple)
       |
       |def _scpy_isinstance_empty_tuple(x):
       |    return isinstance(x, tuple) and len(x) == 0
       |
       |def _scpy_isinstance_nonempty_tuple(x):
       |    return isinstance(x, tuple) and len(x) > 0
       |
       |# Polymorphic `Product` method dispatch. After erasure, a Tuple value
       |# whose static type is `Product`/`Any`/match-type-aliased is just a
       |# Python tuple, with no Scala-level Product methods. These helpers
       |# branch on `isinstance(x, tuple)` and otherwise dispatch to the
       |# encoded Product method on the real receiver.
       |def _scpy_product_arity(p):
       |    if isinstance(p, tuple):
       |        return len(p)
       |    return p.${m("productArity")(I)}()
       |
       |def _scpy_product_element(p, n):
       |    if isinstance(p, tuple):
       |        if n < 0 or n >= len(p):
       |            raise IndexOutOfBoundsException(_builtins.str(n))
       |        return p[n]
       |    return p.${m("productElement", I)(ObjRef)}(n)
       |
       |def _scpy_product_iterator(p):
       |    if isinstance(p, tuple):
       |        return _scpy_tuple_iter(p)
       |    return p.${m("productIterator")(PyClassRef(PyClassName("scala.collection.Iterator")))}()
       |
       |def _scpy_product_prefix(p):
       |    if isinstance(p, tuple):
       |        return _scpy_tuple_prefix(p)
       |    return p.${m("productPrefix")(StrRef)}()
       |
       |def _scpy_product_element_name(p, n):
       |    if isinstance(p, tuple):
       |        return _scpy_tuple_element_name(p, n)
       |    return p.${m("productElementName", I)(StrRef)}(n)
       |""".stripMargin
