package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import java.io.PrintWriter
import scala.collection.mutable

/** Emits Python source text from a list of `PyClassDef`s.
 *
 *  Consumes the typed PyIR and produces a single bundled Python file
 *  with an inline runtime preamble. Called from `GenPython.linkAndWrite`.
 *
 *  Responsibilities:
 *    - `@staticmethod` decoration driven by `PyMemberFlags.namespace`
 *    - `self` parameter insertion for instance methods
 *    - Module-singleton instantiation for `PyClassKind.ModuleClass`
 *    - Main-guard synthesis from a `(PyClassName, PyClassKind)` entry
 *      - looks up the actual `main` method by simple name and invokes
 *      it with the correct encoded signature
 *    - Float NaN/Inf literal fix (`float('nan')` / `float('inf')`)
 *    - Control-character string escaping
 *    - Fully-qualified module variable names (no cross-package
 *      module-name collision)
 *    - Mechanical numeric wrapping (`_scpy_i32` / `_scpy_i64` /
 *      `_scpy_f32`) driven by the op code
 *    - `PyMatch` lowering to nested `if/elif` (stmt) or ternary (expr)
 */
/** Thrown when the emitter sees a PyIR shape it does not know how to
 *  render. Linker / reachability normalize emitter inputs, so any bug
 *  reaching the emitter is a real backend bug, not a missing feature. */
final class EmitterBug(message: String) extends RuntimeException(message)

object PyIREmitter:

  /** Reserved prefix for compiler-invented Python identifiers. */
  val Prefix: String = "_scpy_"

  /** Scala FQCNs whose simple name collides with a Python builtin and
   *  must be remapped at emission time (see `Emitter.classIdentifier`).
   *
   *  Only `java.lang.Exception` is problematic today: we emit
   *  `class Throwable(Exception):` against Python's builtin, so the
   *  Scala `class Exception(Throwable):` definition would rebind
   *  `Exception` in module scope. Keep this narrow — a user-defined
   *  Scala class named `Exception` in a different package stays free to
   *  emit `class Exception(...)` normally. Other potential collisions
   *  like `java.lang.Error` are not Python builtins, so they pass
   *  through.
   */
  private val PythonReservedShortNames: Map[String, String] = Map(
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

  /** Entry point for a "main" method: the class where it lives plus
   *  that class's kind (Class vs ModuleClass - determines whether the
   *  guard calls `Name.main(...)` or `_scpy_mod_Name.main(...)`). */
  type MainEntry = (PyClassName, PyClassKind)

  private sealed trait ExternImport
  private final case class ModuleImport(module: String) extends ExternImport
  private final case class FromImport(module: String, name: String) extends ExternImport

  /** Emit a bundle: runtime preamble + all classes + optional main
   *  entry point, into the given writer.
   *
   *  When `mainEntry` is provided, the emitter looks up a method named
   *  `main` (by simple name) on the given class and synthesizes an
   *  `if __name__ == "__main__":` guard calling it with `sys.argv[1:]`
   *  iff it takes any parameters.
   */
  def emit(
      classes:   List[PyClassDef],
      mainEntry: Option[MainEntry],
      out:       PrintWriter
  ): Unit =
    val e = new Emitter(out)
    e.emitBundle(classes, mainEntry)

  /** Convenience for producing the Python source as a `String`. */
  def emitToString(
      classes:   List[PyClassDef],
      mainEntry: Option[MainEntry]
  ): String =
    val buf = new java.io.StringWriter()
    val pw  = new PrintWriter(buf)
    try emit(classes, mainEntry, pw)
    finally pw.flush()
    buf.toString

  // ===============================================================
  //  Private emitter state machine
  // ===============================================================

  private class Emitter(out: PrintWriter):
    private var indentLevel: Int = 0
    private val indentStr: String = "    "
    private val externAliases = mutable.LinkedHashMap.empty[ExternImport, String]
    private var knownClasses: Set[PyClassName] = Set.empty
    private var classByName: Map[PyClassName, PyClassDef] = Map.empty

    // Per-method state for `PyLabeled` / `PyLabelReturn` emission. Each
    // Labeled that survives the usage-detection pre-pass allocates one
    // `BaseException` subclass (`_scpy_lbl_<n>`) and records the
    // mapping here; `PyLabelReturn` looks up the class by label and
    // emits `raise <class>(<value>)`. Reset at every method entry
    // because the `_scpy_lbl_<n>` names must be unique within one
    // Python `def`. See `notes/issue-labeled-*.md`.
    private var labelClassCounter: Int = 0
    private val labelClasses = mutable.Map.empty[PyLabelName, String]

    private def indent(): Unit = indentLevel += 1
    private def dedent(): Unit = indentLevel -= 1

    private def line(s: String): Unit =
      if s.isEmpty then out.println()
      else
        out.print(indentStr * indentLevel)
        out.println(s)

    private def emptyLine(): Unit = out.println()

    // -- Entry point ---------------------------------------------

    def emitBundle(classes: List[PyClassDef], mainEntry: Option[MainEntry]): Unit =
      collectExternAliases(classes)
      // Track the set of nominal class names whose Python definitions
      // actually exist in this bundle (or in the runtime preamble).
      // `buildBasesList` uses this to filter out interfaces with no
      // Python representation.
      knownClasses =
        classes.iterator.map(_.name).toSet ++ PyIRRuntime.providedClasses.keySet
      classByName = classes.iterator.map(c => c.name -> c).toMap

      val orderedClasses = topoSortClasses(classes)

      line("# Generated by Scala 3 Python backend (PyIR)")
      emptyLine()

      // Runtime preamble - defines numeric wrappers, Predef, etc.
      out.print(PyIRRuntime.content)
      emptyLine()

      emitExternImports()
      if externAliases.nonEmpty then
        emptyLine()

      // Class definitions + runtime registration. Emit in a topological
      // order so that each class's Python base(s) are already defined
      // above. Python evaluates `class Foo(Bar):` at definition time and
      // resolves `Bar` by name — forward references fail with
      // `NameError`.
      for cls <- orderedClasses do
        line(s"# -- ${cls.name.nameString} --")
        emptyLine()
        emitClassDef(cls)
        emitClassRegistration(cls)
        emptyLine()

      // Instantiate Scala `object`s only after every class body in the
      // bundle has been defined. Module constructors can reference
      // synthetic classes emitted later in the same bundle (e.g. Scala 3
      // enum case implementations); eager per-class initialization would
      // see those names before they exist.
      //
      // Lazy init via `_scpy_LazyModule` (see PyIRRuntime). Each
      // `_scpy_mod_<name>_` binding is a `_scpy_LazyModule` proxy that
      // wraps the underlying class instance allocated with `__new__()`
      // (no constructor call yet). On the first attribute access through
      // the proxy — read, call, attribute write, or `_scpy_module_value`
      // unwrap — `_scpy_ensure()` runs the real `__init__()` exactly
      // once. This breaks reference cycles between modules because the
      // proxy's name resolves immediately, and any access path that
      // routes through the LazyModule (`PyApplyStatic`, `PyLoadModule`,
      // `PyApply` against a module receiver) implicitly fires the
      // initializer before reading the member.
      //
      // INVARIANT: every public `<module>.<member>` access path in the
      // emitter must go through `moduleAccessExpr` / `moduleValueExpr`,
      // never through the bare class identifier when a singleton exists.
      // `hasNoModuleVarBinding` gates the bare-class fallback to the
      // shapes where no singleton was emitted (non-ModuleClass classes
      // with no `_`-suffixed companion).
      //
      // Only top-level module classes are eligible for singleton init.
      // Inner module classes (e.g. `object Enumeration.ValueSet` whose
      // companion is `scala.Enumeration.ValueSet_`) take an `_outer`
      // argument and can't be instantiated without one. Detect by
      // looking for a no-arg constructor — top-level Scala objects have
      // one; inner objects don't.
      val moduleClasses = orderedClasses.filter { cls =>
        cls.kind == PyClassKind.ModuleClass &&
          cls.methods.exists(m =>
            m.flags.namespace == PyMemberNamespace.Constructor && m.args.isEmpty
          )
      }

      if moduleClasses.nonEmpty then
        // Lazy module init: every singleton is allocated via `__new__`
        // (no ctor call). Each module's `__init__` is wrapped so it
        // runs at most once on first attribute access (via a `__getattr__`
        // hook installed below). This breaks the module-init dependency
        // cascade — modules that nothing in user code touches never
        // initialize, and modules that DO get touched chain their inits
        // in the order accesses happen.
        line("# -- module singletons (lazy-init) --")
        emptyLine()
        for cls <- moduleClasses do
          val modVar = moduleVarName(cls.name)
          val clsId  = classIdentifier(cls.name)
          line(s"$modVar = _scpy_lazy_module($clsId)")
        emptyLine()

      mainEntry.foreach(entry => emitMainGuard(entry, orderedClasses))

    // -- Class definition ----------------------------------------

    private def emitClassDef(cls: PyClassDef): Unit =
      val bases = buildBasesList(cls)
      val basesStr = if bases.isEmpty then "" else s"(${bases.mkString(", ")})"
      line(s"class ${classIdentifier(cls.name)}$basesStr:")
      indent()

      val ctorMethods = cls.methods.filter(_.flags.namespace == PyMemberNamespace.Constructor)
      val nonCtorMethods = cls.methods.filter(_.flags.namespace != PyMemberNamespace.Constructor)
      val needsSyntheticInit = ctorMethods.isEmpty && cls.fields.nonEmpty

      var first = true
      def spacer(): Unit =
        if first then first = false else emptyLine()

      spacer()
      emitClassMetadata(cls)

      // For module classes (singleton state), emit class-level None
      // defaults for every field. Two-phase init creates singletons via
      // `__new__` first, then runs `__init__` in topo order; cross-module
      // accesses during another module's `__init__` may hit a not-yet-
      // initialized field — Python would raise AttributeError. Java's
      // semantics for uninitialized static fields are "null", so a class-
      // level `None` default matches and breaks the cascade.
      if cls.kind == PyClassKind.ModuleClass && cls.fields.nonEmpty then
        for f <- cls.fields do
          line(s"${f.name.encoded} = ${classLevelFieldInitExpr(f)}")

      // Emit a no-arg `__init__(self)` (used by module lazy-init, and as
      // a Python-required entry point on `cls()` calls that the runtime
      // never makes from generated code) plus one helper per ctor
      // overload. `PyNew` lowering at call sites picks the right helper
      // by encoded signature; there is no runtime overload guessing.
      if ctorMethods.nonEmpty then
        spacer()
        emitNoArgInitForwarder(cls, ctorMethods)
        for ctor <- ctorMethods do
          spacer()
          emitMethodDef(cls, ctor)
      else if needsSyntheticInit then
        spacer()
        emitSyntheticInit(cls)

      // Emit the remaining (non-constructor) methods
      for m <- nonCtorMethods do
        spacer()
        emitMethodDef(cls, m)

      // Python data-model rule: a class that defines `__eq__` but not
      // `__hash__` has `__hash__` implicitly set to `None`, even if a
      // base class defines `__hash__`. That makes instances unhashable
      // and `obj.__hash__()` raise `TypeError: 'NoneType' object is not
      // callable`. Scala's contract is the opposite: every `Object`
      // subclass has a `hashCode`, and overriding `equals` does NOT
      // wipe out an inherited `hashCode`. Re-thread the inherited slot
      // so `tm.hashCode` works on classes like `immutable.TreeMap` that
      // override `equals` while inheriting `hashCode` from `Map`.
      maybeRebindInheritedHash(cls)

      // If the class has no members at all, emit `pass`
      if first then line("pass")

      dedent()

    /** If `cls` emitted `__eq__` locally but did not emit `__hash__`,
     *  add `__hash__ = <Ancestor>.__hash__` so Python doesn't shadow
     *  the inherited hash with `None`. The ancestor is the first class
     *  in the MRO (`superClass` first, then declared interfaces) that
     *  has its own `__hash__` method, walking transitively through the
     *  bundle and runtime-provided classes. Falls back to `_scpy_Object`
     *  (i.e. Python `object`'s identity hash) when no ancestor defines
     *  one explicitly — that's still callable, so the slot is no longer
     *  `None`.
     */
    private def maybeRebindInheritedHash(cls: PyClassDef): Unit =
      val hasLocalEq   = cls.methods.exists(m => methodEmitsAsDunder(m, "__eq__"))
      val hasLocalHash = cls.methods.exists(m => methodEmitsAsDunder(m, "__hash__"))
      if hasLocalEq && !hasLocalHash then
        val ancestor = findAncestorWithHash(cls).getOrElse(PyClassName.ObjectClass)
        line(s"__hash__ = ${classIdentifier(ancestor)}.__hash__")

    /** True iff `m` would be emitted as the Python dunder `name` (e.g.
     *  `__eq__`, `__hash__`). Constructor methods take a different
     *  emission path so they are excluded. */
    private def methodEmitsAsDunder(m: PyMethodDef, name: String): Boolean =
      m.flags.namespace != PyMemberNamespace.Constructor
        && m.name.simple.name == name

    /** Walk `cls`'s ancestor chain (bundle + runtime-provided) breadth-
     *  first and return the first class that locally defines `__hash__`.
     *  `_scpy_Object` is treated as not having a Scala `__hash__` (the
     *  prelude doesn't write one — Python falls back to `object.__hash__`
     *  via MRO, which is fine, but we don't return it here so callers
     *  default to `_scpy_Object`/`PyClassName.ObjectClass` only when
     *  nothing better exists). */
    private def findAncestorWithHash(cls: PyClassDef): Option[PyClassName] =
      val seen = mutable.Set.empty[PyClassName]
      val queue = mutable.Queue.empty[PyClassName]
      // Seed with direct parents in the bases-list order so the choice
      // matches Python's MRO walk for simple linearizations.
      cls.superClass.foreach(queue.enqueue)
      cls.interfaces.foreach(queue.enqueue)
      while queue.nonEmpty do
        val n = queue.dequeue()
        if seen.add(n) then
          if classDefinesHash(n) then return Some(n)
          classByName.get(n) match
            case Some(c) =>
              c.superClass.foreach(queue.enqueue)
              c.interfaces.foreach(queue.enqueue)
            case None =>
              PyIRRuntime.providedClass(n).foreach { pc =>
                pc.superClass.foreach(queue.enqueue)
                pc.interfaces.foreach(queue.enqueue)
              }
      None

    /** True iff the bundle class `name` locally defines a `__hash__`
     *  method. Runtime-provided classes don't carry method bodies that
     *  the emitter can rebind to, so they're never "hash-defining" for
     *  this purpose; the `_scpy_Object` fallback covers that path. */
    private def classDefinesHash(name: PyClassName): Boolean =
      classByName.get(name).exists { c =>
        c.methods.exists(m => methodEmitsAsDunder(m, "__hash__"))
      }

    private def emitClassMetadata(cls: PyClassDef): Unit =
      line("_scpy_full_name = \"" + escapeString(cls.name.nameString) + "\"")
      line("_scpy_kind = \"" + classKindLiteral(cls.kind) + "\"")
      line("_scpy_superclass = " + classSuperclassLiteral(cls))
      line("_scpy_interfaces = " + classInterfacesLiteral(cls.interfaces))
      line("def getClass__Ljava_dlang_dClass(self):")
      indent()
      line("return _scpy_class_of_instance(self)")
      dedent()

    private def emitClassRegistration(cls: PyClassDef): Unit =
      val clsId = classIdentifier(cls.name)
      line(s"_scpy_register_class($clsId, $clsId._scpy_full_name, $clsId._scpy_kind, $clsId._scpy_superclass, $clsId._scpy_interfaces)")

    private def classKindLiteral(kind: PyClassKind): String = kind match
      case PyClassKind.Interface => "interface"
      case _                     => "class"

    private def classSuperclassLiteral(cls: PyClassDef): String =
      cls.kind match
        case PyClassKind.Interface =>
          "None"
        case _ =>
          val superName = cls.superClass.getOrElse(PyClassName.ObjectClass)
          "\"" + escapeString(superName.nameString) + "\""

    private def classInterfacesLiteral(interfaces: List[PyClassName]): String =
      if interfaces.isEmpty then "()"
      else
        interfaces
          .map(cn => "\"" + escapeString(cn.nameString) + "\"")
          .mkString("(", ", ", if interfaces.length == 1 then ",)" else ")")

    /** Topologically sort `classes` so that any class whose Python bases
     *  (superclass + materialised interfaces) appear in the bundle are
     *  emitted after those bases. Classes whose bases are runtime-provided
     *  or not in `knownClasses` impose no ordering constraint from those
     *  edges. The sort is stable in the original bundle order for ties,
     *  preserving the grouping-by-source-file that the linker provides.
     */
    private def topoSortClasses(classes: List[PyClassDef]): List[PyClassDef] =
      val classesByName = classes.iterator.map(c => c.name -> c).toMap
      val order = new java.util.LinkedHashMap[PyClassName, PyClassDef]
      val inProgress = mutable.Set.empty[PyClassName]

      def visit(cls: PyClassDef): Unit =
        if order.containsKey(cls.name) then ()
        else if inProgress(cls.name) then
          // Cycle - fall back to the original position so we still emit
          // every class (Python will raise NameError at runtime if the
          // cycle is load-bearing; pre-SAM-expansion this didn't happen).
          ()
        else
          inProgress += cls.name
          val bases = cls.superClass.toList ::: cls.interfaces
          for base <- bases do
            classesByName.get(base).foreach(visit)
          inProgress -= cls.name
          order.put(cls.name, cls)

      for cls <- classes do visit(cls)
      import scala.jdk.CollectionConverters.*
      order.values().asScala.toList

    private def buildBasesList(cls: PyClassDef): List[String] =
      if cls.name == PyClassName.ThrowableClass then
        // A source-ported `java.lang.Throwable` must remain a real Python
        // exception or `raise` / `except Exception` stop working.
        return List("Exception")

      // Scala superclass plus any interface whose Python class actually
      // exists in the bundle (or is runtime-provided). Traits without a
      // Python representation — scala stdlib internals like
      // `scala.deriving.Mirror.Product` — are silently dropped: Python
      // is duck-typed so the nominal membership is unnecessary for those.
      // Real traits carrying default methods (e.g. ported javalib
      // interfaces like `java.util.function.Predicate`) must flow through
      // so anonymous SAM expansions can `super()` into the default
      // implementations.
      val directSuper = cls.superClass.toList.filterNot(_ == PyClassName.ObjectClass)
      val ifaceBases  = cls.interfaces.filter(knownClasses.contains)
      val rawBases    = (directSuper ::: ifaceBases).distinct

      // Drop bases that are already transitive ancestors of another base.
      // Python's C3 linearization rejects `class C(A, B)` when `B extends A`
      // (e.g. our `MarkerCloseable extends java.io.Closeable`: Scala reports
      // `AutoCloseable + Closeable` as direct parents, but Python needs just
      // `Closeable` because it already inherits `AutoCloseable`).
      val redundant = rawBases.flatMap(other => transitiveAncestors(other) - other).toSet
      val bases = rawBases.filterNot(redundant.contains).map(classIdentifier)
      if bases.isEmpty then List("_scpy_Object") else bases

    /** True if `cls` is `java.lang.Throwable` or transitively extends it,
     *  following both in-bundle super/interface links and the
     *  runtime-provided whitelist. Used to gate `emitClassMetadata`
     *  (see the call site). */
    private def isThrowableDescendant(cls: PyClassDef): Boolean =
      transitiveAncestors(cls.name).contains(PyClassName.ThrowableClass)

    /** All transitive ancestors of `name` reachable through classes in the
     *  bundle (`classInfos`) or the runtime-provided whitelist. Includes
     *  `name` itself so callers can detect the diagonal. */
    private def transitiveAncestors(name: PyClassName): Set[PyClassName] =
      val seen = mutable.Set.empty[PyClassName]
      def walk(n: PyClassName): Unit =
        if seen.add(n) then
          classByName.get(n) match
            case Some(cls) =>
              cls.superClass.foreach(walk)
              cls.interfaces.foreach(walk)
            case None =>
              PyIRRuntime.providedClass(n).foreach { pc =>
                pc.superClass.foreach(walk)
                pc.interfaces.foreach(walk)
              }
      walk(name)
      seen.toSet

    private def emitSyntheticInit(cls: PyClassDef): Unit =
      line("def __init__(self) -> None:")
      indent()
      if cls.fields.isEmpty then
        line("pass")
      else
        for f <- cls.fields do
          line(s"self.${f.name.encoded} = ${fieldInitExpr(f)}")
      dedent()

    /** Emit a `def __init__(self, *args)` that JVM-style zero-initializes
     *  every instance field (`null`/`0`/`false` defaults), and — only
     *  when called with non-empty `args` — dispatches to the unique
     *  ctor helper of matching arity. The arity dispatch is a
     *  convenience fallback for runtime/prelude code that uses Python's
     *  `cls(args)` protocol; Scala-emitted call sites bypass `__init__`
     *  entirely by going through `_scpy_new(cls, _scpy_ctor_<sig>,
     *  args...)`, which selects the helper at codegen time by encoded
     *  signature.
     *
     *  Note: arity dispatch is a strictly weaker form than the old
     *  type-guard dispatcher — it correctly handles the common case of
     *  exception classes constructed from the prelude
     *  (`NullPointerException("msg")`, `ArithmeticException("/ by
     *  zero")`, etc.), where each exception class has at most one
     *  ctor per arity. It does NOT attempt to disambiguate between
     *  multiple same-arity ctors; if a Scala class has overloads like
     *  `(Int)` and `(String)`, the Scala-side call site is expected to
     *  go through `_scpy_new` (which it does, automatically — `PyNew`
     *  always lowers to `_scpy_new`).
     */
    private def emitNoArgInitForwarder(cls: PyClassDef, ctors: List[PyMethodDef]): Unit =
      line("def __init__(self, *args) -> None:")
      indent()
      // Field zero-init: every instance attribute starts at the JVM
      // default for its declared type.
      for f <- cls.fields do
        line(s"self.${f.name.encoded} = ${fieldInitExpr(f)}")
      // Group ctors by arity. For each non-empty arity, emit a helper
      // call only when there is exactly one ctor at that arity (so
      // Python-protocol `cls(args)` can resolve unambiguously).
      val byArity = ctors.groupBy(_.args.length)
      // Order arities so stable code, but emit nothing for arity 0:
      // the empty-args path is just the zero-init above.
      val nonEmptyArities = byArity.keysIterator.filter(_ != 0).toList.sorted
      if nonEmptyArities.nonEmpty then
        line("if _scpy_len(args) == 0:")
        indent(); line("return None"); dedent()
        for arity <- nonEmptyArities do
          val matching = byArity(arity)
          if matching.size == 1 then
            val ctor = matching.head
            val forwarded =
              (0 until arity).map(i => s"args[$i]").mkString(", ")
            line(s"if _scpy_len(args) == $arity:")
            indent()
            line(s"self.${constructorHelperName(cls.name, ctor.name)}($forwarded)")
            line("return None")
            dedent()
      // No matching arity (or multiple ctors at this arity — the
      // codegen path through `_scpy_new` is the supported one):
      // silently leave the instance zero-initted. This matches what
      // happens when the linker has DCE'd the relevant helper.
      if cls.fields.isEmpty && nonEmptyArities.isEmpty then
        line("pass")
      dedent()

    private def fieldDefaultExpr(tpe: PyType): String = tpe match
      case PyBooleanType => "False"
      case PyByteType | PyShortType | PyCharType | PyIntType | PyLongType => "0"
      case PyFloatType | PyDoubleType => "0.0"
      case _ => "None"

    /** Suffix appended by the `LazyVals` mini-phase to the per-lazy-val
     *  static `VarHandle` symbol name. The container's mangled name plus
     *  this suffix is what reaches us as a Python field name (with `$`
     *  rewritten to `_` by `sanitizeName`). */
    private val LazyHandleSuffix: String = "_lzyHandle"

    /** Default-init expression for `f`, with special handling for the
     *  per-lazy-val `<container>_lzyHandle` field synthesized by the
     *  `LazyVals` mini-phase.
     *
     *  On the JVM, the static `findVarHandle(...)` rhs runs in `<clinit>`,
     *  which has no Python analogue; the backend currently drops the rhs
     *  in `genClassMembers` and the field zero-inits to `None`, then
     *  `compareAndSet` blows up at the first lazy access (see
     *  `notes/issue-list-range-lazyhandle-none.md`). The simple, single-
     *  threaded fix: synthesize a `VarHandle` carrying the underlying
     *  container field name (`<container>_lzyHandle` -> container
     *  `<container>`) so the per-instance `compareAndSet` calls have a
     *  real receiver. */
    private def fieldInitExpr(f: PyFieldDef): String =
      // The simple-name suffix detects the lazy-val handle field; the
      // *container* attribute it operates on is the encoded name of
      // the matching lazy-val storage field (so post-mangling the
      // VarHandle's `compareAndSet` reaches the same Python attribute
      // the lazy-init reads from / writes to).
      val simple = f.name.simple.name
      if simple.endsWith(LazyHandleSuffix) then
        val container = f.name.encoded.stripSuffix(LazyHandleSuffix)
        s"_scpy_make_lazy_handle(\"${container}\")"
      else
        fieldDefaultExpr(f.ftpe)

    /** Class-level field default for module-class singletons. Mirrors
     *  `fieldInitExpr` for the lazy-val handle case but otherwise falls
     *  back to the legacy `None` literal so the cross-module-init
     *  cascade (see the call site) keeps the JVM-style "uninitialized
     *  static field reads as null" semantics. */
    private def classLevelFieldInitExpr(f: PyFieldDef): String =
      val simple = f.name.simple.name
      if simple.endsWith(LazyHandleSuffix) then
        val container = f.name.encoded.stripSuffix(LazyHandleSuffix)
        s"_scpy_make_lazy_handle(\"${container}\")"
      else
        "None"

    private def constructorHelperName(owner: PyClassName, name: PyMethodName): String =
      val paramPart =
        if name.paramTypeRefs.isEmpty then "void"
        else name.paramTypeRefs.map(_.encoded).mkString("_")
      s"_scpy_ctor_${classIdentifier(owner)}__${paramPart}__${name.resultTypeRef.encoded}"

    // -- Method definition ---------------------------------------

    private def emitMethodDef(cls: PyClassDef, method: PyMethodDef): Unit =
      // Per-method state for labeled-escape emission: each Python `def`
      // gets a fresh `_scpy_lbl_<n>` namespace.
      labelClassCounter = 0
      labelClasses.clear()

      // Decorator for static methods
      if method.flags.namespace == PyMemberNamespace.PublicStatic
         || method.flags.namespace == PyMemberNamespace.PrivateStatic then
        line("@staticmethod")

      val methodPyName =
        if method.flags.namespace == PyMemberNamespace.Constructor then
          constructorHelperName(cls.name, method.name)
        else
          method.name.encoded
      val paramsList   = buildParamList(method)
      val retAnnot     = pythonReturnAnnot(method.resultType)
      line(s"def $methodPyName($paramsList)$retAnnot:")

      indent()
      method.body match
        case None =>
          methodPyName match
            case "__eq__" if method.args.nonEmpty =>
              line(s"return self is ${method.args.head.name.name}")
            case "__hash__" =>
              line("return _scpy_identity_hash_code(self)")
            case _ =>
              line("raise NotImplementedError()")
        case Some(body) =>
          val stmts = flattenBlock(body)
          val isVoidReturn = method.resultType == PyVoidType
                          || method.resultType == PyNothingType
                          || method.flags.namespace == PyMemberNamespace.Constructor
          emitMethodBody(stmts, returnLast = !isVoidReturn)
      dedent()

    private def buildParamList(method: PyMethodDef): String =
      val selfParam: List[String] =
        if method.flags.namespace.isInstance
           || method.flags.namespace == PyMemberNamespace.Constructor
        then List("self")
        else Nil
      val userParams = method.args.map { p =>
        val annot = pythonTypeAnnot(p.ptpe) match
          case ""    => ""
          case "Any" => ""
          case a     => s": $a"
        s"${p.name.name}$annot"
      }
      (selfParam ++ userParams).mkString(", ")

    private def pythonReturnAnnot(t: PyType): String =
      pythonTypeAnnot(t) match
        case ""    => ""
        case "Any" => ""  // skip noisy untyped annotation
        case s     => s" -> $s"

    private def pythonTypeAnnot(t: PyType): String = t match
      case PyVoidType | PyNothingType | PyNullType => "None"
      case PyBooleanType => "bool"
      case PyCharType | PyByteType | PyShortType | PyIntType | PyLongType => "int"
      case PyFloatType | PyDoubleType => "float"
      case PyStringType => "str"
      case PyArrayType  => "list"
      // Drop class-type annotations - they would be forward references
      // to identifiers the class/module is still in the middle of defining.
      case PyClassType(_) => ""
      case PyAnyType => ""

    private def emitMethodBody(stmts: List[PyTree], returnLast: Boolean): Unit =
      if stmts.isEmpty then
        line("pass")
      else if !returnLast then
        stmts.foreach(emitStmt)
      else
        for s <- stmts.init do emitStmt(s)
        wrapLastReturn(stmts.last)

    /** Turn the last statement of a method body into a `return`. For
     *  nested structures (blocks, if/else, try/catch), recurse into
     *  every exit path so each one is wrapped.
     *
     *  `methodReturnLabels` tracks labels whose escape the peephole
     *  has collapsed into a direct Python `return`. When a
     *  `PyLabeled(label, body)` is tail-targeted (every
     *  `PyLabelReturn(label, _)` inside is at a tail position per
     *  this function's own recursion), we skip the try/except wrapper
     *  and the `_scpy_lbl_<n>` class allocation, and emit the inner
     *  `PyLabelReturn`s as plain Python `return`s. Matches the
     *  previous placeholder behaviour on the common "match at method
     *  tail" shape — no exception machinery, no per-call class
     *  creation. */
    private def wrapLastReturn(
        tree: PyTree,
        methodReturnLabels: Set[PyLabelName] = Set.empty
    ): Unit = tree match
      case PyBlock(stats, expr) =>
        stats.foreach(emitStmt)
        wrapLastReturn(expr, methodReturnLabels)

      case PyIf(cond, thenp, elsep) =>
        line(s"if ${exprToStr(cond)}:")
        indent(); wrapLastReturn(thenp, methodReturnLabels); dedent()
        val elseStmts = flattenBlock(elsep)
        if elseStmts.nonEmpty then
          line("else:")
          indent(); wrapLastReturn(elsep, methodReturnLabels); dedent()

      case PyTryCatch(block, errVar, _, handler) =>
        line("try:")
        indent(); wrapLastReturn(block, methodReturnLabels); dedent()
        line(s"except Exception as ${errVar.name}:")
        indent(); wrapLastReturn(handler, methodReturnLabels); dedent()

      case PyTryFinally(block, finalizer) =>
        line("try:")
        indent(); wrapLastReturn(block, methodReturnLabels); dedent()
        line("finally:")
        indent(); emitBlockStmts(finalizer); dedent()

      case PyMatch(selector, cases, default) =>
        val selStr = exprToStr(selector)
        if cases.isEmpty then
          wrapLastReturn(default, methodReturnLabels)
        else
          var first = true
          for (lits, body) <- cases do
            val keyword = if first then "if" else "elif"
            first = false
            val conds = lits.map(lit => s"$selStr == ${exprToStr(lit)}").mkString(" or ")
            line(s"$keyword $conds:")
            indent(); wrapLastReturn(body, methodReturnLabels); dedent()
          line("else:")
          indent(); wrapLastReturn(default, methodReturnLabels); dedent()

      // Method-tail peephole: the Labeled wraps the method's tail
      // expression and every escape to its label is at a tail
      // position. Emit body without the try/except wrapper; nested
      // `PyLabelReturn(label, _)` emit as plain `return <value>`.
      case PyLabeled(label, body) if isTailTargeted(body, label) =>
        wrapLastReturn(body, methodReturnLabels + label)

      // Peepholed label return → plain Python return.
      case PyLabelReturn(label, value) if methodReturnLabels.contains(label) =>
        value match
          case _: PyUnitLit => line("return")
          case PyBlock(stats, tail) =>
            stats.foreach(emitStmt)
            tail match
              case _: PyUnitLit => line("return")
              case _            => line(s"return ${exprToStr(tail)}")
          case _ => line(s"return ${exprToStr(value)}")

      case _: PyUnitLit =>
        line("return None")

      case _ if tree.tpe == PyVoidType || tree.tpe == PyNothingType =>
        emitStmt(tree)

      case _ =>
        line(s"return ${exprToStr(tree)}")

    // -- Statements ----------------------------------------------

    private def emitStmt(tree: PyTree): Unit = tree match
      case PyVarDef(name, _, _, _, rhs) =>
        line(s"${name.name} = ${exprToStr(rhs)}")

      case PyAssign(lhs, rhs) =>
        line(s"${exprToStr(lhs)} = ${exprToStr(rhs)}")

      case PyReturn(value) =>
        // Flatten a trailing `PyBlock` so its `stats` (side-effect
        // statements hoisted from a Unit-typed expression, e.g. the arm
        // body of a match) run before the `return`. `exprToStr(PyBlock)`
        // silently drops stats — see `notes/issue-return-drops-unit-
        // side-effects.md`.
        value match
          case _: PyUnitLit => line("return")
          case PyBlock(stats, tail) =>
            stats.foreach(emitStmt)
            tail match
              case _: PyUnitLit => line("return")
              case _            => line(s"return ${exprToStr(tail)}")
          case assign: PyAssign =>
            // Defence-in-depth: same swallow shape as `emitRaiseLabel` —
            // a `PyAssign` returned as the value of a `PyReturn` would
            // otherwise degrade to `"None"` via `exprToStr`. Emit it as a
            // statement, then a bare `return`.
            emitStmt(assign)
            line("return")
          case _            => line(s"return ${exprToStr(value)}")

      case PyWhile(cond, body) =>
        line(s"while ${exprToStr(cond)}:")
        indent()
        emitBlockStmts(body)
        dedent()

      case PySkip() =>
        line("pass")

      case PyBlock(stats, expr) =>
        stats.foreach(emitStmt)
        emitStmt(expr)

      case PyIf(cond, thenp, elsep) =>
        line(s"if ${exprToStr(cond)}:")
        indent()
        emitBlockStmts(thenp)
        dedent()
        val elseStmts = flattenBlock(elsep)
        if elseStmts.nonEmpty then
          line("else:")
          indent()
          elseStmts.foreach(emitStmt)
          dedent()

      case PyTryCatch(block, errVar, _, handler) =>
        line("try:")
        indent()
        emitBlockStmts(block)
        dedent()
        line(s"except Exception as ${errVar.name}:")
        indent()
        emitBlockStmts(handler)
        dedent()

      case PyTryFinally(block, finalizer) =>
        line("try:")
        indent()
        emitBlockStmts(block)
        dedent()
        line("finally:")
        indent()
        emitBlockStmts(finalizer)
        dedent()

      case PyLabeled(label, body) =>
        // If no `PyLabelReturn(label, _)` inside the body targets this
        // label, the wrapper is dead weight — emit body inline.
        // PatternMatcher produces this shape whenever all match arms
        // fall through (no explicit escape), e.g. a Unit-typed match
        // whose arms only run for side effects.
        if !labelReturnTargets(body, label) then
          emitBlockStmts(body)
        else
          val className = allocLabelClass(label)
          emitLabelClassDecl(className)
          line("try:")
          indent(); emitBlockStmts(body); dedent()
          line(s"except $className:")
          indent(); line("pass"); dedent()
          labelClasses.remove(label)  // name is scoped to this Labeled

      case PyLabelReturn(label, value) =>
        // Lookup the class allocated by the enclosing `PyLabeled`. If
        // absent it means either (a) lowering produced a `PyLabelReturn`
        // whose target is the method-return label being peepholed by
        // `wrapLastReturn`, which must intercept before reaching here,
        // or (b) a structural bug. Fall back to a `return` for (a)'s
        // edge case; (b) would surface a `NameError` at runtime so the
        // assertion below catches it at emit time.
        labelClasses.get(label) match
          case Some(className) =>
            emitRaiseLabel(className, value)
          case None =>
            throw AssertionError(
              s"PyLabelReturn targeting unallocated label '${label.name}' — " +
              "emission must allocate the class via the enclosing `PyLabeled`, " +
              "or `wrapLastReturn` must peephole the return before reaching " +
              "`emitStmt(PyLabelReturn)`."
            )

      case PyMatch(selector, cases, default) =>
        val selStr = exprToStr(selector)
        if cases.isEmpty then
          emitBlockStmts(default)
        else
          var first = true
          for (lits, body) <- cases do
            val keyword = if first then "if" else "elif"
            first = false
            val conds = lits.map(lit => s"$selStr == ${exprToStr(lit)}").mkString(" or ")
            line(s"$keyword $conds:")
            indent(); emitBlockStmts(body); dedent()
          val defaultStmts = flattenBlock(default)
          if defaultStmts.nonEmpty then
            line("else:")
            indent(); defaultStmts.foreach(emitStmt); dedent()

      case _: PyUnitLit =>
        ()  // unit in statement position is a no-op

      case expr =>
        // Fall-through: expression used as a statement
        line(exprToStr(expr))

    /** Flatten a tree into a list of statements and emit them. */
    private def emitBlockStmts(tree: PyTree): Unit =
      val stmts = flattenBlock(tree)
      if stmts.isEmpty then line("pass")
      else stmts.foreach(emitStmt)

    // -- Labeled escape machinery --------------------------------
    //
    // `PyLabeled(label, body)` emits a unique `_scpy_lbl_<n>` subclass
    // of `BaseException` plus a `try/except` wrapper; every
    // `PyLabelReturn(label, v)` inside the body lowers to
    // `raise _scpy_lbl_<n>(<v>)`. Nested labels don't interfere
    // because each label has its own class; outer-label raises pass
    // through inner `except` clauses unhandled. Matches Scala.js's
    // use of JavaScript's labeled `break` but adapted for Python.
    //
    // `BaseException` (not `Exception`) so user code using
    // `except Exception` cannot accidentally swallow our escapes.

    private def allocLabelClass(label: PyLabelName): String =
      labelClasses.get(label) match
        case Some(existing) => existing
        case None =>
          labelClassCounter += 1
          val className = s"_scpy_lbl_$labelClassCounter"
          labelClasses.update(label, className)
          className

    private def emitLabelClassDecl(className: String): Unit =
      line(s"class $className(BaseException):")
      indent()
      line("__slots__ = (\"value\",)")
      line("def __init__(self, v=None): self.value = v")
      dedent()

    private def emitRaiseLabel(className: String, value: PyTree): Unit =
      // A `PyBlock` here carries the hoisted side effects that the
      // lowering-side fix for `Return(_, Block([stats], ()))` pushed
      // into the return value. Emit the stats, then the raise with
      // the tail expression (or no argument if tail is Unit).
      value match
        case PyBlock(stats, tail) =>
          stats.foreach(emitStmt)
          tail match
            case _: PyUnitLit => line(s"raise $className()")
            case _            => line(s"raise $className(${exprToStr(tail)})")
        case assign: PyAssign =>
          // Defence-in-depth: a `PyAssign` returned as the value of a
          // label-escape would otherwise hit the `case other => "None"`
          // swallow in `exprToStr` and silently drop the side effect.
          // Emit the assignment as a statement, then a no-arg raise.
          emitStmt(assign)
          line(s"raise $className()")
        case _: PyUnitLit =>
          line(s"raise $className()")
        case _ =>
          line(s"raise $className(${exprToStr(value)})")

    /** True iff any `PyLabelReturn(target, _)` appears inside `tree`. */
    private def labelReturnTargets(tree: PyTree, target: PyLabelName): Boolean =
      tree match
        case PyLabelReturn(lbl, value) =>
          lbl == target || labelReturnTargets(value, target)
        case PyBlock(stats, expr) =>
          stats.exists(labelReturnTargets(_, target)) ||
          labelReturnTargets(expr, target)
        case PyIf(cond, thenp, elsep) =>
          labelReturnTargets(cond, target) ||
          labelReturnTargets(thenp, target) ||
          labelReturnTargets(elsep, target)
        case PyTryCatch(block, _, _, handler) =>
          labelReturnTargets(block, target) ||
          labelReturnTargets(handler, target)
        case PyTryFinally(block, finalizer) =>
          labelReturnTargets(block, target) ||
          labelReturnTargets(finalizer, target)
        case PyMatch(selector, cases, default) =>
          labelReturnTargets(selector, target) ||
          cases.exists { case (_, body) => labelReturnTargets(body, target) } ||
          labelReturnTargets(default, target)
        case PyLabeled(_, body) =>
          labelReturnTargets(body, target)
        case PyWhile(cond, body) =>
          labelReturnTargets(cond, target) || labelReturnTargets(body, target)
        case PyAssign(_, rhs) =>
          labelReturnTargets(rhs, target)
        case PyVarDef(_, _, _, _, rhs) =>
          labelReturnTargets(rhs, target)
        case PyReturn(value) =>
          labelReturnTargets(value, target)
        case _ => false

    /** True iff every `PyLabelReturn(target, _)` inside `tree` sits at
     *  a tail position per `wrapLastReturn`'s own recursion rules.
     *  Used to decide whether a method-tail `PyLabeled` can be
     *  peepholed: rather than allocating a class + try/except we let
     *  the nested `PyLabelReturn`s emit a plain `return <value>`.
     *
     *  Conservative — any `PyLabelReturn(target, _)` outside a tail
     *  position disables the peephole for this label. */
    private def isTailTargeted(tree: PyTree, target: PyLabelName): Boolean =
      tree match
        case PyLabelReturn(lbl, value) =>
          lbl == target && !labelReturnTargets(value, target)
        case PyBlock(stats, expr) =>
          !stats.exists(labelReturnTargets(_, target)) &&
          isTailTargeted(expr, target)
        case PyIf(_, thenp, elsep) =>
          isTailTargeted(thenp, target) && isTailTargeted(elsep, target)
        case PyTryCatch(block, _, _, handler) =>
          isTailTargeted(block, target) && isTailTargeted(handler, target)
        case PyTryFinally(block, finalizer) =>
          isTailTargeted(block, target) && !labelReturnTargets(finalizer, target)
        case PyMatch(_, cases, default) =>
          cases.forall { case (_, body) => isTailTargeted(body, target) } &&
          isTailTargeted(default, target)
        case PyLabeled(_, body) =>
          // Nested Labeled — the inner's body is a tail position too.
          isTailTargeted(body, target)
        case _ =>
          // Any leaf tree that isn't a PyLabelReturn to `target` and
          // doesn't contain one is trivially tail-targeted (no
          // offending return inside).
          !labelReturnTargets(tree, target)

    private def bindExternAlias(imp: ExternImport): String =
      externAliases.getOrElseUpdate(imp, {
        val raw = imp match
          case ModuleImport(module)    => module
          case FromImport(module, name) => s"${module}_${name}"
        s"${Prefix}ext_${raw.replace('.', '_')}"
      })

    private def importOf(module: String, path: List[String]): (ExternImport, List[String]) =
      if path.isEmpty then (ModuleImport(module), Nil)
      else (FromImport(module, path.head), path.tail)

    private def collectExternAliases(classes: List[PyClassDef]): Unit =
      for
        cls <- classes
        method <- cls.methods
        body <- method.body
      do collectExternAliases(body)

    private def collectExternAliases(tree: PyTree): Unit = tree match
      case tree: PyVarDef =>
        collectExternAliases(tree.rhs)
      case tree: PyAssign =>
        collectExternAliases(tree.lhs)
        collectExternAliases(tree.rhs)
      case tree: PyReturn =>
        collectExternAliases(tree.value)
      case tree: PyWhile =>
        collectExternAliases(tree.cond)
        collectExternAliases(tree.body)
      case _: PySkip =>
        ()
      case tree: PyIf =>
        collectExternAliases(tree.cond)
        collectExternAliases(tree.thenp)
        collectExternAliases(tree.elsep)
      case tree: PyTryCatch =>
        collectExternAliases(tree.block)
        collectExternAliases(tree.handler)
      case tree: PyTryFinally =>
        collectExternAliases(tree.block)
        collectExternAliases(tree.finalizer)
      case tree: PyMatch =>
        collectExternAliases(tree.selector)
        tree.cases.foreach { case (_, body) => collectExternAliases(body) }
        collectExternAliases(tree.default)
      case tree: PyBlock =>
        tree.stats.foreach(collectExternAliases)
        collectExternAliases(tree.expr)
      case tree: PyLabeled =>
        collectExternAliases(tree.body)
      case tree: PyLabelReturn =>
        collectExternAliases(tree.value)
      case _: PyVarRef | _: PyThis | _: PySelectStatic | _: PyLiteral | _: PyClassOf | _: PyLoadModule =>
        ()
      case tree: PySelect =>
        collectExternAliases(tree.qualifier)
      case tree: PyApply =>
        collectExternAliases(tree.receiver)
        tree.args.foreach(collectExternAliases)
      case tree: PyApplyStatically =>
        collectExternAliases(tree.receiver)
        tree.args.foreach(collectExternAliases)
      case tree: PyApplyStatic =>
        tree.args.foreach(collectExternAliases)
      case tree: PyApplyExternal =>
        tree.args.foreach(collectExternAliases)
      case tree: PyExternalRef =>
        val (imp, _) = importOf(tree.module, tree.path)
        bindExternAlias(imp)
      case tree: PyApplyDynamic =>
        collectExternAliases(tree.callee)
        tree.args.foreach(collectExternAliases)
        tree.kwargs.foreach((_, value) => collectExternAliases(value))
      case tree: PyAttrAccess =>
        collectExternAliases(tree.obj)
      case tree: PyNew =>
        tree.args.foreach(collectExternAliases)
      case tree: PyIsInstanceOf =>
        collectExternAliases(tree.expr)
      case tree: PyAsInstanceOf =>
        collectExternAliases(tree.expr)
      case tree: PyNewArray =>
        collectExternAliases(tree.length)
      case tree: PyArrayValue =>
        tree.elems.foreach(collectExternAliases)
      case tree: PyArraySelect =>
        collectExternAliases(tree.array)
        collectExternAliases(tree.index)
      case tree: PyUnaryOp =>
        collectExternAliases(tree.lhs)
      case tree: PyBinaryOp =>
        collectExternAliases(tree.lhs)
        collectExternAliases(tree.rhs)
      case tree: PyClosure =>
        collectExternAliases(tree.body)

    private def emitExternImports(): Unit =
      for (imp, alias) <- externAliases do
        imp match
          case ModuleImport(module) =>
            line(s"import $module as $alias")
          case FromImport(module, name) =>
            line(s"from $module import $name as $alias")

    // -- Expressions ---------------------------------------------

    private def exprToStr(tree: PyTree): String = tree match
      // Literals
      case PyBooleanLit(true)  => "True"
      case PyBooleanLit(false) => "False"
      case PyCharLit(c)        => c.toInt.toString
      case PyByteLit(v)        => v.toString
      case PyShortLit(v)       => v.toString
      case PyIntLit(v)         => v.toString
      case PyLongLit(v)        => v.toString
      case PyFloatLit(v)       => formatFloat(v)
      case PyDoubleLit(v)      => formatDouble(v)
      case PyStringLit(v)      => "\"" + escapeString(v) + "\""
      case PyNullLit()         => "None"
      case PyUnitLit()         => "None"

      // References
      case PyVarRef(name)      => name.name
      case PyThis()            => "self"
      case PySelect(qual, field) =>
        s"${parenthesize(qual)}.${field.encoded}"
      case PySelectStatic(field) =>
        s"${classIdentifier(field.owner)}.${field.encoded}"

      // Calls
      case PyApply(_, receiver, className, method, args) =>
        val argsStr = args.map(exprToStr).mkString(", ")
        // C7: a `this(...)` self-delegation inside a secondary ctor
        // body reaches us as `PyApply(this, OwnerClass, <init>, args)`.
        // Emitting `self.__init__(args)` would re-enter the dynamic
        // type's dispatcher — re-zero fields, re-run type guards — and
        // subclasses' dispatcher-shape changes could misdispatch. Skip
        // the dispatcher entirely and call the specific primary helper
        // directly. Use the SAME helper-name function the definition
        // uses (`constructorHelperName`) so names stay in lockstep.
        if method.simple.isConstructor && receiver.isInstanceOf[PyThis] then
          s"self.${constructorHelperName(className, method)}($argsStr)"
        else
          s"${parenthesize(receiver)}.${method.encoded}($argsStr)"

      case PyApplyStatically(_, receiver, className, method, args) =>
        val argsStr = args.map(exprToStr).mkString(", ")
        // Bypass Python's MRO and dispatch directly to the resolved
        // class. `PyApplyStatically.className` already names the exact
        // class in which the method is resolved (per PyIR docs), so
        // we honour Scala's source-level resolution rather than letting
        // Python C3 walk pick a different override.
        //
        // Why not `super()`? Python's MRO and Scala's linearization
        // disagree in two directions:
        //   1. Trait re-overrides: `class C extends B with T` linearizes
        //      T after B in Scala (T wins); Python MRO with bases `(B, T)`
        //      walks B before T (B wins). Reversing the bases tuple to
        //      `(T, B)` flips this case but breaks
        //   2. Abstract trait declarations: `class C extends B with T`
        //      where T declares `m` abstractly and B implements it.
        //      Scala's linearization preserves B's concrete impl; Python
        //      with `(T, B)` would pick T's abstract declaration.
        // Calling `ClassName.method(self, args)` sidesteps both: we use
        // exactly the class the Scala compiler resolved.
        //
        // See notes/issue-arraydeque-map-class-walk-recursion.md.
        // Constructor calls land here for `super.<init>(...)` chains.
        // Route them to the encoded ctor helper directly so the parent's
        // `__init__` (a no-arg field-zero-init forwarder) is bypassed —
        // we want exactly the helper that matches the resolved
        // signature. Runtime-provided classes (e.g. `_scpy_Object`,
        // `_scpy_java_Throwable`, the `*Ref` boxes) don't carry the
        // encoded helpers — fall back to `__init__` (which on
        // `object`/`_scpy_Object` is a no-op, and on the hand-written
        // runtime stubs is the actual ctor body). Ditto for support
        // classes that escaped the bundle (e.g. interfaces with no
        // ctor body), in which case the dispatcher form remains a
        // safe fallback.
        val targetMethod =
          if method.simple.isConstructor then
            if PyIRRuntime.providedClass(className).isDefined then method.encoded
            else constructorHelperName(className, method)
          else method.encoded
        val sep = if args.isEmpty then "" else ", "
        receiver match
          case _: PyThis =>
            s"${classIdentifier(className)}.$targetMethod(self$sep$argsStr)"
          case _ =>
            val prefix = parenthesize(receiver)
            s"${classIdentifier(className)}.$targetMethod($prefix$sep$argsStr)"

      case PyApplyStatic(_, className, method, args) =>
        val argsStr = args.map(exprToStr).mkString(", ")
        // Same rerouting policy as PyLoadModule: stdlib's static method
        // calls on `java.util.Arrays` (a bare class) need to land on
        // pylib's `java.util.Arrays_` module singleton. But when the
        // target class itself is a non-ModuleClass in the bundle and has
        // no `_`-suffixed companion module class — e.g. trait-static
        // helpers (`loop$2` lifted onto
        // `scala.collection.StrictOptimizedLinearSeqOps`) or JVM-static
        // helpers on `final class`es (`ArrayBufferView.superArg_1`) —
        // there is no `_scpy_mod_<className>_` binding. The static method
        // is already attached to the class with `@staticmethod`, so call
        // it directly on the class identifier.
        //
        // Additionally, if the named class itself is a non-ModuleClass
        // and locally defines this `@staticmethod` (e.g. a class-level
        // lifted lambda like `TreeMap#filter$$anonfun$1`), dispatch
        // there directly instead of routing to the underscored
        // companion module — which exists in the bundle but does not
        // carry that helper. See
        // `notes/issue-treemap-filter-anonfun-wrong-owner.md`.
        if hasNoModuleVarBinding(className) then
          s"${classIdentifier(className)}.${method.encoded}($argsStr)"
        else if hasOwnStaticMethod(className, method) then
          s"${classIdentifier(className)}.${method.encoded}($argsStr)"
        else
          s"${moduleAccessExpr(routeToModuleVar(className))}.${method.encoded}($argsStr)"

      case PyApplyExternal(callee, args) =>
        val argsStr = args.map(exprToStr).mkString(", ")
        s"${callee.name}($argsStr)"

      case PyExternalRef(module, path) =>
        val (imp, rest) = importOf(module, path)
        val alias = bindExternAlias(imp)
        if rest.isEmpty then alias
        else s"$alias.${rest.mkString(".")}"

      case PyApplyDynamic(callee, args, kwargs) =>
        val posArgs = args.map(exprToStr)
        val kwArgs = kwargs.map((name, value) => s"$name=${exprToStr(value)}")
        s"${parenthesize(callee)}(${(posArgs ++ kwArgs).mkString(", ")})"

      case PyAttrAccess(obj, name) =>
        s"${parenthesize(obj)}.$name"

      // Construction. Emit `_scpy_new(Cls, Cls._scpy_ctor_<sig>, args...)`
      // (a runtime helper defined in `PyIRRuntime.prelude`) so the right
      // ctor overload is picked at codegen time by symbol identity. No
      // runtime arity / type-guard dispatch.
      //
      // Runtime-provided classes (e.g. `_scpy_Object`, the `*Ref` boxes,
      // `_scpy_Class`, runtime-provided `Throwable` parents) don't carry
      // the encoded `_scpy_ctor_*` helpers — the prelude handles their
      // ctors via Python's standard `__init__` protocol. For those, fall
      // back to `Cls(args...)`, which calls `Cls.__new__` + `Cls.__init__`.
      case PyNew(className, ctor, args) =>
        val clsId   = classIdentifier(className)
        val argsStr = args.map(exprToStr).mkString(", ")
        if PyIRRuntime.providedClass(className).isDefined then
          s"$clsId($argsStr)"
        else
          val helper  = constructorHelperName(className, ctor)
          val argList = (s"$clsId" :: s"$clsId.$helper" :: args.map(exprToStr)).mkString(", ")
          s"_scpy_new($argList)"

      case PyLoadModule(className) =>
        // When the target is a non-ModuleClass class in the bundle (no
        // `_scpy_mod_*_` binding will be produced — see the `moduleClasses`
        // filter in emitPreamble), fall back to the class identifier.
        // The class itself is already defined in module scope and can
        // serve as the carrier for `@staticmethod`-decorated helpers,
        // which is the only thing a `LoadModule` of a non-module is used
        // for downstream.
        if hasNoModuleVarBinding(className) then
          classIdentifier(className)
        else
          moduleValueExpr(routeToModuleVar(className))

      // Type tests / casts
      case PyIsInstanceOf(expr, testType) =>
        s"_scpy_is_value_of_type(${exprToStr(expr)}, ${typeRefToClassExpr(testType)})"

      case PyAsInstanceOf(expr, target) =>
        // Python is duck-typed, so most reference casts are no-ops. The
        // primitive cases need explicit lowering to match JVM unboxing:
        //
        // - Char: a boxed `Character` is `_scpy_Char` (an `int` subclass);
        //   unbox to the raw codepoint so downstream Char ops and the
        //   `toString` hook see a plain `int`. NPE on null (matches
        //   `BoxesRunTime.unboxToChar(null)`).
        // - Other primitives (Z/B/S/I/J/F/D): JVM `BoxesRunTime.unboxTo*`
        //   on `null` returns the primitive default, not NPE. Route
        //   through `_scpy_unbox_or_default(tag, value)`, which is a
        //   no-op when the value is already a primitive and substitutes
        //   the default when it is `None` (e.g. `null.asInstanceOf[Int]`
        //   reaching us via an erased generic call). Same helper used
        //   by `genClosure` for the SAM-bridge null-unbox path.
        target match
          case PyCharType    => s"_scpy_unbox_char(${exprToStr(expr)})"
          case PyBooleanType => s"""_scpy_unbox_or_default("Z", ${exprToStr(expr)})"""
          case PyByteType    => s"""_scpy_unbox_or_default("B", ${exprToStr(expr)})"""
          case PyShortType   => s"""_scpy_unbox_or_default("S", ${exprToStr(expr)})"""
          case PyIntType     => s"""_scpy_unbox_or_default("I", ${exprToStr(expr)})"""
          case PyLongType    => s"""_scpy_unbox_or_default("J", ${exprToStr(expr)})"""
          case PyFloatType   => s"""_scpy_unbox_or_default("F", ${exprToStr(expr)})"""
          case PyDoubleType  => s"""_scpy_unbox_or_default("D", ${exprToStr(expr)})"""
          case _             => exprToStr(expr)

      // Arrays
      case PyNewArray(elemTypeRef, length) =>
        s"_scpy_new_array(${typeRefToClassExpr(elemTypeRef)}, ${parenthesize(length)}, ${arrayDefaultValue(elemTypeRef)})"

      case PyArrayValue(elemTypeRef, elems) =>
        s"_scpy_array_value(${typeRefToClassExpr(elemTypeRef)}, [${elems.map(exprToStr).mkString(", ")}])"

      case PyArraySelect(array, index) =>
        s"${parenthesize(array)}[${exprToStr(index)}]"

      // Operators
      case PyUnaryOp(op, lhs) =>
        emitUnary(op, lhs)

      case PyBinaryOp(op, lhs, rhs) =>
        emitBinary(op, lhs, rhs)

      // Closures (simplified). Wrapped in an arity-specific
      // `_scpy_FnN` subclass (defined in the runtime preamble for
      // `N = 0..22`) that both inherits the `_scpy_Fn` `__call__` /
      // `apply*` forwarding and extends the matching nominal `FunctionN`
      // base, so runtime `_scpy_is_instance(closure, scala.FunctionN)`
      // succeeds at constructor-dispatch sites with a `FunctionN`
      // parameter (e.g. `IndexedSeqView.Map(self, f)`). Arities above 22
      // fall back to the plain `_scpy_Fn` carrier; Scala's source
      // language only defines `FunctionN` for `N <= 22`, so this is just
      // a safety net for codegen-synthesized closures of unexpected
      // shape.
      case PyClosure(params, _, body) =>
        val paramsStr = params.map(_.name.name).mkString(", ")
        val arity = params.length
        val carrier = if arity >= 0 && arity <= 22 then s"_scpy_Fn$arity" else "_scpy_Fn"
        s"$carrier(lambda $paramsStr: ${exprToStr(body)})"

      case PyClassOf(typeRef) =>
        typeRefToClassExpr(typeRef)

      // If-expression
      case PyIf(cond, thenp, elsep) =>
        s"(${exprToStr(thenp)} if ${exprToStr(cond)} else ${exprToStr(elsep)})"

      case PyMatch(selector, cases, default) =>
        // Lower to nested ternary: each case is `body if cond else <else>`.
        // Statement-shaped arm bodies are forbidden here for the same
        // reason as in `PyIf` — see the `PyBlock` case below.
        val selStr = exprToStr(selector)
        cases.foldRight(exprToStr(default)) { case ((lits, body), elsePart) =>
          val conds = lits.map(lit => s"$selStr == ${exprToStr(lit)}").mkString(" or ")
          s"(${exprToStr(body)} if ($conds) else $elsePart)"
        }

      // INVARIANT: `PyBlock`, `PyLabeled`, `PyLabelReturn`,
      // `PyTryCatch`, and `PyTryFinally` are statement-shaped nodes
      // and must never appear in expression position. Rendering them
      // as a Python expression would silently drop their statements
      // (the `result = ...` assignments inside the block body), which
      // produces broken code where the result variable is read but
      // never bound — see
      // `notes/issue-list-vector-large-literal-unbound-locals.md`.
      // The `GenPython` lowering hoists such trees to a temp + a
      // statement; any occurrence here is a backend bug.
      case _: (PyBlock | PyLabeled | PyLabelReturn | PyTryCatch | PyTryFinally) =>
        throw new AssertionError(
          s"statement-shaped PyIR node in expression position: ${tree.getClass.getSimpleName} " +
          s"at ${tree.pos}; this must be hoisted by GenPython before reaching the emitter."
        )

      case other =>
        // Hard failure: any node reaching this arm is one the emitter
        // doesn't know how to render in expression position. The linker
        // and reachability normalize their inputs, so a hit here is a
        // backend bug — silently emitting `None` (the previous
        // behaviour) hides the bug and produces Python that runs but
        // returns the wrong value.
        throw new EmitterBug(
          s"unhandled PyIR node in expression position: " +
          s"${other.getClass.getSimpleName} at ${other.pos}; " +
          s"render=${other.toString.take(160)}"
        )

    /** Wrap an expression in parentheses if its precedence requires it. */
    private def parenthesize(tree: PyTree): String = tree match
      case _: PyBinaryOp | _: PyUnaryOp | _: PyIf | _: PyClosure =>
        s"(${exprToStr(tree)})"
      case _ =>
        exprToStr(tree)

    // -- Unary / binary op rendering -----------------------------

    private def emitUnary(op: PyUnaryCode, lhs: PyTree): String =
      import PyUnaryCode.*
      val l = parenthesize(lhs)
      op match
        case BoolNot      => s"(not $l)"
        case IntNeg       => wrapI32(s"-$l")
        case LongNeg      => wrapI64(s"-$l")
        case FloatNeg     => wrapF32(s"-$l")
        case DoubleNeg    => s"(-$l)"
        case IntNot       => wrapI32(s"~$l")
        case LongNot      => wrapI64(s"~$l")

        case CharToInt | ByteToInt | ShortToInt | IntToLong => l
        case LongToInt => wrapI32(l)
        case IntToFloat | LongToFloat | DoubleToFloat => wrapF32(s"float($l)")
        case IntToDouble | LongToDouble | FloatToDouble => s"float($l)"
        case IntToChar  => s"(($l) & 0xFFFF)"
        case IntToByte  => s"((($l) & 0xFF) ^ 0x80) - 0x80"
        case IntToShort => s"((($l) & 0xFFFF) ^ 0x8000) - 0x8000"
        case FloatToInt | DoubleToInt => wrapI32(s"int($l)")
        case FloatToLong | DoubleToLong => wrapI64(s"int($l)")

        case ArrayLength    => s"_scpy_len($l)"

        case Throw          => s"(lambda: (_ for _ in ()).throw($l))()"

    private def emitBinary(op: PyBinaryCode, lhs: PyTree, rhs: PyTree): String =
      import PyBinaryCode.*
      val l = parenthesize(lhs)
      val r = parenthesize(rhs)
      op match
        // Boolean
        case BoolEq => s"($l == $r)"
        case BoolNe => s"($l != $r)"
        case BoolOr => s"($l or $r)"
        case BoolAnd => s"($l and $r)"

        // Int
        case IntAdd  => wrapI32(s"$l + $r")
        case IntSub  => wrapI32(s"$l - $r")
        case IntMul  => wrapI32(s"$l * $r")
        // Truncate-toward-zero semantics matching JVM `idiv` / `irem`.
        // Python's `//` and `%` are floor-style, so a runtime helper
        // adjusts the sign for negative operands.
        case IntDiv  => wrapI32(s"${Prefix}int_trunc_div($l, $r)")
        case IntMod  => wrapI32(s"${Prefix}int_trunc_mod($l, $r)")
        case IntOr   => wrapI32(s"$l | $r")
        case IntAnd  => wrapI32(s"$l & $r")
        case IntXor  => wrapI32(s"$l ^ $r")
        // JVM masks shift amount to low 5 bits for `int`. Python's
        // arbitrary-precision shift does no such masking, so e.g.
        // `1 << 32` yields 4294967296 instead of 1. Mask explicitly.
        case IntShl  => wrapI32(s"$l << ($r & 0x1F)")
        case IntShr  => wrapI32(s"$l >> ($r & 0x1F)")
        case IntUShr => wrapI32(s"${Prefix}int_ushr32($l, $r)")
        case IntEq   => s"($l == $r)"
        case IntNe   => s"($l != $r)"
        case IntLt   => s"($l < $r)"
        case IntLe   => s"($l <= $r)"
        case IntGt   => s"($l > $r)"
        case IntGe   => s"($l >= $r)"

        // Long
        case LongAdd  => wrapI64(s"$l + $r")
        case LongSub  => wrapI64(s"$l - $r")
        case LongMul  => wrapI64(s"$l * $r")
        // Same truncation rule as Int / wrap to 64-bit.
        case LongDiv  => wrapI64(s"${Prefix}int_trunc_div($l, $r)")
        case LongMod  => wrapI64(s"${Prefix}int_trunc_mod($l, $r)")
        case LongOr   => wrapI64(s"$l | $r")
        case LongAnd  => wrapI64(s"$l & $r")
        case LongXor  => wrapI64(s"$l ^ $r")
        // JVM masks shift amount to low 6 bits for `long`. Python's
        // arbitrary-precision shift gives `1L << 64 == 2^64`, which
        // gets normalised by `_scpy_i64` to `0` rather than the JVM
        // value `1L`. That silently loses bits in BitSet's `1L << elem`
        // when `elem >= 64` lands on a higher word. Mask explicitly.
        case LongShl  => wrapI64(s"$l << ($r & 0x3F)")
        case LongShr  => wrapI64(s"$l >> ($r & 0x3F)")
        case LongUShr => wrapI64(s"${Prefix}int_ushr64($l, $r)")
        case LongEq   => s"($l == $r)"
        case LongNe   => s"($l != $r)"
        case LongLt   => s"($l < $r)"
        case LongLe   => s"($l <= $r)"
        case LongGt   => s"($l > $r)"
        case LongGe   => s"($l >= $r)"

        // Float
        case FloatAdd => wrapF32(s"$l + $r")
        case FloatSub => wrapF32(s"$l - $r")
        case FloatMul => wrapF32(s"$l * $r")
        case FloatDiv => wrapF32(s"$l / $r")
        case FloatMod => wrapF32(s"$l % $r")
        case FloatEq => s"($l == $r)"
        case FloatNe => s"($l != $r)"
        case FloatLt => s"($l < $r)"
        case FloatLe => s"($l <= $r)"
        case FloatGt => s"($l > $r)"
        case FloatGe => s"($l >= $r)"

        // Double
        case DoubleAdd => s"($l + $r)"
        case DoubleSub => s"($l - $r)"
        case DoubleMul => s"($l * $r)"
        case DoubleDiv => s"($l / $r)"
        case DoubleMod => s"($l % $r)"
        case DoubleEq => s"($l == $r)"
        case DoubleNe => s"($l != $r)"
        case DoubleLt => s"($l < $r)"
        case DoubleLe => s"($l <= $r)"
        case DoubleGt => s"($l > $r)"
        case DoubleGe => s"($l >= $r)"

        // String
        case StringConcat => s"($l + $r)"
        case StringEq => s"($l == $r)"

        // Identity
        case RefEq => s"($l is $r)"
        case RefNe => s"($l is not $r)"

    private def arrayDefaultValue(elemTypeRef: PyTypeRef): String = elemTypeRef match
      case PyPrimRef(tag) => tag match
        case PyPrimRef.Tag.BooleanRef => "False"
        case PyPrimRef.Tag.FloatRef   => "0.0"
        case PyPrimRef.Tag.DoubleRef  => "0.0"
        case PyPrimRef.Tag.VoidRef    => "None"
        case PyPrimRef.Tag.NullRef    => "None"
        case PyPrimRef.Tag.NothingRef => "None"
        case _                        => "0"
      case _ =>
        "None"

    private def typeRefToClassExpr(typeRef: PyTypeRef): String = typeRef match
      case PyPrimRef(tag) => tag match
        case PyPrimRef.Tag.VoidRef    => s"${Prefix}primitive_void"
        case PyPrimRef.Tag.BooleanRef => s"${Prefix}primitive_boolean"
        case PyPrimRef.Tag.CharRef    => s"${Prefix}primitive_char"
        case PyPrimRef.Tag.ByteRef    => s"${Prefix}primitive_byte"
        case PyPrimRef.Tag.ShortRef   => s"${Prefix}primitive_short"
        case PyPrimRef.Tag.IntRef     => s"${Prefix}primitive_int"
        case PyPrimRef.Tag.LongRef    => s"${Prefix}primitive_long"
        case PyPrimRef.Tag.FloatRef   => s"${Prefix}primitive_float"
        case PyPrimRef.Tag.DoubleRef  => s"${Prefix}primitive_double"
        case PyPrimRef.Tag.NullRef | PyPrimRef.Tag.NothingRef =>
          s"""${Prefix}class_of_name("java.lang.Object")"""
      case PyClassRef(cn) =>
        s"""${Prefix}class_of_name("${escapeString(cn.nameString)}")"""
      case PyArrayRef(base, dims) =>
        (0 until dims).foldLeft(typeRefToClassExpr(base)) { (componentExpr, _) =>
          s"${Prefix}array_class($componentExpr)"
        }

    // -- Numeric wrapping helpers --------------------------------

    private def wrapI32(inner: String): String = s"${Prefix}i32($inner)"
    private def wrapI64(inner: String): String = s"${Prefix}i64($inner)"
    private def wrapF32(inner: String): String = s"${Prefix}f32($inner)"

    // -- Module singleton init -----------------------------------

    private def emitModuleSingletonInit(cls: PyClassDef): Unit =
      val modVar = moduleVarName(cls.name)
      val clsId  = classIdentifier(cls.name)
      line(s"$modVar = $clsId()")

    // -- Main guard ----------------------------------------------

    private def emitMainGuard(mainEntry: MainEntry, classes: List[PyClassDef]): Unit =
      val (mainClass, kind) = mainEntry
      val methodOpt = classes.find(_.name == mainClass).flatMap { cls =>
        cls.methods.find(_.name.simple.name == "main")
      }
      methodOpt.foreach { m =>
        val encoded = m.name.encoded
        val argsStr = if m.args.nonEmpty then "sys.argv[1:]" else ""
        val receiver = kind match
          case PyClassKind.ModuleClass => moduleVarName(mainClass)
          case _                       => classIdentifier(mainClass)
        line("import sys")
        line("if __name__ == \"__main__\":")
        indent()
        line(s"$receiver.$encoded($argsStr)")
        dedent()
      }

    // -- Name helpers --------------------------------------------

    /** Python identifier used for a Scala class (simple name, sanitized).
     *
     *  A handful of Scala FQCNs collide with Python builtins when reduced
     *  to their simple name. `java.lang.Exception` is the load-bearing
     *  case: we emit `class Throwable(Exception):` where `Exception` is
     *  Python's builtin (see `buildBasesList`), then the topological
     *  emission later produces the Scala `class Exception(Throwable):`,
     *  which would REBIND `Exception` in module scope and shadow the
     *  builtin for any subsequent `except Exception:` in generated
     *  Python. Remap the FQCN (not the simple name — a user class literally
     *  named `Exception` in a different package would legitimately emit
     *  `class Exception(...)`) to a mangled identifier.
     */
    private def classIdentifier(cn: PyClassName): String =
      PyIREmitter.PythonReservedShortNames.get(cn.nameString) match
        case Some(alias) => alias
        case None =>
          // Runtime-provided classes keep the simple name so they line
          // up with hand-written declarations in the prelude
          // (`class Function1`, `class Mirror`, etc.). User-emitted
          // classes use the mangled FQN to avoid collisions when
          // multiple packages export classes with the same simple name
          // (e.g. `scala.collection.SortedSetOps` vs
          // `scala.collection.mutable.SortedSetOps` vs
          // `scala.collection.immutable.SortedSetOps` — all three would
          // otherwise emit as `class SortedSetOps:` and shadow each
          // other, breaking Python MRO).
          if PyIRRuntime.providedClass(cn).isDefined then
            sanitizeIdent(cn.simpleName)
          else
            cn.segments.map(sanitizeIdent).mkString("_")

    /** Python variable holding the singleton of a Scala `object`.
     *  Uses the full qualified path so two modules with the same
     *  simple name in different packages don't collide. */
    private def moduleVarName(cn: PyClassName): String =
      s"${Prefix}mod_${cn.segments.map(sanitizeIdent).mkString("_")}_"

    /** Always go through the moduleVarName for now; the prelude
     *  already binds `_scpy_mod_*_` for runtime-provided ModuleClasses
     *  (Int_, Char_, BoxedUnit) so the variable name resolves at
     *  runtime. */
    private def moduleAccessExpr(cn: PyClassName): String =
      moduleVarName(cn)

    private def moduleValueExpr(cn: PyClassName): String =
      s"_scpy_module_value(${moduleAccessExpr(cn)})"

    /** Rewrite a class-style ClassName to its module-class counterpart
     *  when only the latter has a singleton in this bundle. Stdlib
     *  emits `LoadModule(java.util.Arrays)` (the synthetic forwarder
     *  class) for static-method access, but our singletons live on
     *  `java.util.Arrays_` (the Scala module class). Returns `cn`
     *  unchanged if either (a) `cn` is itself a ModuleClass or
     *  (b) there's no `_`-suffixed counterpart. Used by both
     *  `PyLoadModule` and `PyApplyStatic` emission. */
    private def routeToModuleVar(cn: PyClassName): PyClassName =
      classByName.get(cn) match
        case Some(c) if c.kind == PyClassKind.ModuleClass => cn
        case _ =>
          val underscored = PyClassName(cn.nameString + "_")
          classByName.get(underscored) match
            case Some(c) if c.kind == PyClassKind.ModuleClass => underscored
            case _ => cn

    /** True iff `cn` is known to be a non-ModuleClass in the emitted
     *  bundle and has no `_`-suffixed companion ModuleClass either —
     *  i.e. `_scpy_mod_<cn>_` is *guaranteed* to not be bound. Used to
     *  decide whether `PyApplyStatic` / `PyLoadModule` must call the
     *  `@staticmethod` directly on the class object (Python-level
     *  `Class.method(...)`) instead of routing through the missing
     *  module singleton.
     *
     *  Conservative: if `cn` is not in `classByName` at all (e.g.
     *  runtime-provided classes like `scala.runtime.IntRef` whose
     *  `_scpy_mod_*_` is bound by the prelude), fall through to the
     *  original module-routing path. */
    private def hasNoModuleVarBinding(cn: PyClassName): Boolean =
      classByName.get(cn) match
        case Some(c) if c.kind != PyClassKind.ModuleClass =>
          val underscored = PyClassName(cn.nameString + "_")
          classByName.get(underscored) match
            case Some(uc) if uc.kind == PyClassKind.ModuleClass => false
            case _ => true
        case _ => false

    /** True iff `cn` is a non-ModuleClass in this bundle that locally
     *  defines a `@staticmethod` named `method`. Used by
     *  `PyApplyStatic` rendering to detect class-level lifted helpers
     *  (e.g. `filter$$anonfun$1` lifted onto a value class whose
     *  companion module also exists) that must be dispatched on the
     *  class itself, not on the underscored companion module. */
    private def hasOwnStaticMethod(cn: PyClassName, method: PyMethodName): Boolean =
      classByName.get(cn) match
        case Some(c) if c.kind != PyClassKind.ModuleClass =>
          c.methods.exists { m =>
            (m.flags.namespace == PyMemberNamespace.PublicStatic
              || m.flags.namespace == PyMemberNamespace.PrivateStatic)
            && m.name == method
          }
        case _ => false

    /** Replace `$` with `_`, escape Python keywords. */
    private def sanitizeIdent(s: String): String =
      val cleaned = s.replace('$', '_')
      if PyIRRuntime.PythonKeywords.contains(cleaned) then cleaned + "_"
      else cleaned

    // -- Block flattening ----------------------------------------

    private def flattenBlock(tree: PyTree): List[PyTree] = tree match
      case PyBlock(stats, expr) =>
        stats.flatMap(flattenBlock) ::: flattenBlock(expr)
      case _: PyUnitLit => Nil
      case PySkip()     => Nil
      case _            => List(tree)

    // -- String escaping (control-character safe) ----------------

    private def escapeString(s: String): String =
      val sb = new StringBuilder(s.length + 8)
      var i = 0
      while i < s.length do
        val ch = s.charAt(i)
        ch match
          case '\\' => sb.append("\\\\")
          case '"'  => sb.append("\\\"")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case c if c < 0x20 || c == 0x7f =>
            sb.append("\\x%02x".format(c.toInt))
          case c if c > 0x7f =>
            sb.append("\\u%04x".format(c.toInt))
          case c =>
            sb.append(c)
        i += 1
      sb.toString

    // -- Numeric literal formatting (NaN/Inf safe) ---------------

    private def formatFloat(v: Float): String =
      if java.lang.Float.isNaN(v) then "float('nan')"
      else if v == Float.PositiveInfinity then "float('inf')"
      else if v == Float.NegativeInfinity then "float('-inf')"
      else v.toString

    private def formatDouble(v: Double): String =
      if java.lang.Double.isNaN(v) then "float('nan')"
      else if v == Double.PositiveInfinity then "float('inf')"
      else if v == Double.NegativeInfinity then "float('-inf')"
      else v.toString
