package dotty.tools.backend.python

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import Constants.*
import Contexts.*
import Decorators.*
import Flags.*
import Names.*
import NameKinds.LazyVarHandleName
import NameOps.*
import Phases.*
import Symbols.*
import Types.*
import StdNames.*

import dotty.tools.dotc.report
import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.ScalaPrimitivesOps.*

import dotty.tools.backend.python.ir.pyir.*
import dotty.tools.backend.python.ir.pyir.serialization.{PyIRFormat, PyIRSerializer}

import scala.collection.mutable
import scala.util.boundary, boundary.break

/** Generates Python source files for the compilation unit. */
class GenPython extends Phase:

  override def phaseName: String = GenPython.name

  override def description: String = GenPython.description

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalapy.value

  override def isRunnable(using Context): Boolean =
    super.isRunnable && !ctx.usedBestEffortTasty

  // Per-run queue of pending link tasks. Populated by each unit's `run`
  // (which writes its `.pyir` to the output directory). Drained at the
  // end of `runOn` so every CU's `.pyir` is on disk before any link
  // step calls `PyClasspathLoader.loadSupportInputs`. Without this
  // staging, a CU compiled earlier in the same invocation that
  // references a class declared by a CU compiled later would fail at
  // link time — `Unrolled_1.pyir` from `Unrolled_1.scala` doesn't
  // exist on disk yet when `UnrollTestMain_1.scala` (which sorts ASCII
  // before `Unrolled_1.scala` since `T < _`) runs through GenPython.
  // Mirrors the `.class`-file model on the JVM: pickler/genBCode write
  // bytecode for every CU before any classfile is consumed for
  // resolution. See `notes/issue-genpython-cross-cu-link.md`.
  private val pendingLinks = new mutable.ArrayBuffer[GenPython.PendingLink]

  override def runOn(units: List[CompilationUnit])(using runCtx: Context): List[CompilationUnit] =
    pendingLinks.clear()
    val processed = super.runOn(units)
    drainPendingLinks()
    processed

  override protected def run(using Context): Unit =
    val codegen = new PyCodeGen()
    codegen.runWriteOnly() match
      case Some(pending) => pendingLinks += pending
      case None          => ()

  private def drainPendingLinks()(using ctx: Context): Unit =
    val tasks = pendingLinks.toList
    pendingLinks.clear()
    if !ctx.settings.scpyIrOnly.value then
      for task <- tasks do
        // Each task carries the in-memory User input + the just-written
        // `.pyir` path. Loading Support inputs at this point means every
        // sibling CU's `.pyir` is already on disk; the linker can resolve
        // intra-compile cross-references without depending on filesystem
        // ordering or sort order.
        task.linkAndEmit()

object GenPython:
  val name: String = "genPython"
  val description: String = "generate Python source files"

  /** REPL-mode sink. When installed on the compiler `Context` via
   *  `ReplSinkKey`, `PyCodeGen.runWriteOnly` short-circuits: it skips
   *  writing the `.pyir` file to disk and skips queueing a `PendingLink`
   *  for link-and-emit. Instead it hands the in-memory `PyClassDef`s
   *  straight to the sink, which the Cappy REPL uses to drive its own
   *  per-input link + emit + send-to-subprocess flow.
   *
   *  Bundle (default) mode runs unchanged — no key set, no sink call.
   */
  trait PyReplSink:
    def onCompiled(
        sourceName: String,
        classes:    List[PyClassDef],
        mainEntry:  Option[PyIREmitter.MainEntry]
    ): Unit

  val ReplSinkKey: dotty.tools.dotc.util.Property.Key[PyReplSink] =
    new dotty.tools.dotc.util.Property.Key[PyReplSink]

  /** Runtime-provided shim ancestors that pylib's concrete subclasses do NOT
   *  extend. Calls to these classes' members from a concrete pylib subclass
   *  must be re-anchored to the receiver's static type (see
   *  `PyCodeGen.dispatchOwnerNameOpt`). JDK 25's `StringBuilder` /
   *  `StringBuffer` redeclare inherited methods as `ACC_BRIDGE | ACC_SYNTHETIC`
   *  forwarders; dotc skips those, so without this list reachability would
   *  prune the pylib-supplied implementations.
   */
  private[python] val detachedShimAncestors: Set[PyClassName] = Set(
    PyClassName("java.lang.AbstractStringBuilder")
  )

  /** A deferred link/emit task. Each CU's `run()` produces one
   *  `PendingLink` (see `PyCodeGen.runWriteOnly`). The phase drains
   *  the queue after every CU has written its `.pyir`, so the linker
   *  sees every sibling CU on the classpath.
   *
   *  The compilation-unit `Context` is captured by the implicit at
   *  construction time. `linkAndEmit` reinstates that context so that
   *  diagnostics from `report.error` carry the correct source file.
   */
  private[python] final class PendingLink(
      val sourceName:       String,
      val sourceFile:       dotty.tools.dotc.util.SourceFile,
      val outputDirectory:  dotty.tools.io.AbstractFile,
      val irFile:           dotty.tools.io.AbstractFile,
      val generatedClasses: List[PyClassDef],
      val mainEntry:        Option[PyIREmitter.MainEntry]
  )(using ctx: Context):

    private val savedCtx: Context = ctx

    def linkAndEmit(): Unit = boundary:
      given Context = savedCtx
      val userInput = PyLinker.Input(
        classes   = generatedClasses,
        mainEntry = mainEntry,
        source    = PyLinker.InputSource.User
      )
      val irFileJava = Option(irFile.jpath).map(_.toFile.nn)
      val supportInputs = PyClasspathLoader.loadSupportInputs(excludeOutputFile = irFileJava)
      val linkedBundle =
        try PyLinker.link(List(userInput), supportInputs)
        catch
          case err: PyLinkingException =>
            err.errors.foreach { e =>
              report.error(e.message, sourcePosOf(e.pos))
            }
            break()

      val outfile = outputDirectory.fileNamed(sourceName + ".py")
      val output = outfile.bufferedOutput
      try
        val writer = new java.io.PrintWriter(output)
        try
          PyIREmitter.emit(linkedBundle.classes, linkedBundle.mainEntry, writer)
          writer.flush()
        finally writer.close()
      finally output.close()

    private def sourcePosOf(pos: PyPosition): SourcePosition =
      if pos.isEmpty then NoSourcePosition
      else if sourceFile.path != pos.source then NoSourcePosition
      else
        sourceFile.lineToOffsetOpt(pos.line) match
          case Some(lineOffset) =>
            val offset = (lineOffset + pos.column).max(0).min(sourceFile.length)
            sourceFile.atSpan(Span(offset))
          case None =>
            NoSourcePosition

/** Main code generator that translates post-erasure Scala trees into the typed PyIR.
  */
private class PyCodeGen()(using genCtx: Context):

  private val encoding = new PyEncoding()
  private val pyDefn = PyDefinitions.pydefn
  private val primitives = new ScalaPrimitives(genCtx)
  private val generatedClasses = mutable.ListBuffer.empty[PyClassDef]
  private var mainEntry: Option[PyIREmitter.MainEntry] = None
  private val stringCompanionClassName = PyClassName("java.lang._String_")
  private lazy val charSequenceClass = requiredClassRef("java.lang.CharSequence").symbol.asClass

  /** Centralized table of JDK/runtime intrinsics the backend short-circuits.
   *
   *  Each entry binds a Scala `Symbol` (or, when overload disambiguation is
   *  not available cleanly, an owner+name predicate) to a hand-written PyIR
   *  rewrite. Sites that previously inlined these checks now route through
   *  `intrinsicFor*` lookups so the runtime contract lives in one place.
   *
   *  Symbol identity is preferred over name strings (especially for
   *  `java.lang.reflect.Array.newInstance`, whose owner was previously
   *  matched by a `javaClassName.startsWith` string compare). All looked-up
   *  symbols are cached lazily so the lookup cost is paid once per
   *  compilation unit.
   */
  private object Intrinsics:
    /** `java.lang.reflect.Array` module class — owner of `newInstance`,
     *  `getLength`, etc. Cached so we never fall back to string matching
     *  on `sym.owner.javaClassName`. */
    @scala.annotation.threadUnsafe
    lazy val ArrayReflectModuleClass: Symbol =
      val mod = requiredModule("java.lang.reflect.Array")
      if mod.exists then mod.moduleClass else NoSymbol

    /** True when `sym` is an `Array.newInstance(...)` overload from
     *  `java.lang.reflect.Array`. The overload-specific dispatch (1-D vs
     *  multi-dim) is decided at the call site by inspecting argument
     *  shapes; this predicate just guards entry. */
    def isArrayNewInstance(sym: Symbol): Boolean =
      sym.exists
        && ArrayReflectModuleClass.exists
        && sym.owner == ArrayReflectModuleClass
        && sym.name.toString == "newInstance"

    /** True when `sym` resolves to `java.lang.System` (class or its
     *  synthetic module). Used by the intrinsic dispatcher for
     *  `arraycopy` / `identityHashCode` static calls and for
     *  `out`/`err`/`in` field reads. */
    def isSystemOwner(sym: Symbol): Boolean =
      sym.exists && {
        sym == defn.SystemClass ||
          (defn.SystemModule.exists && sym == defn.SystemModule.moduleClass)
      }

    /** Lookup for static intrinsics that fire from `genNormalApply`'s
     *  static-target branch. Returns the produced PyIR or `None` to fall
     *  through to normal codegen.
     *
     *  Only contains call shapes whose lowering does not need access to
     *  the surrounding `Apply` tree (e.g. for receiver re-anchoring).
     *  String-static and constructor handling stay in their dedicated
     *  helpers since they need name-mangled signatures.
     */
    def applyStaticIntrinsic(
        sym: Symbol,
        args: List[PyTree],
        pos: PyPosition
    ): Option[PyTree] =
      if !isStaticMember(sym) then None
      else if isSystemOwner(sym.owner) then
        sym.name.mangledString match
          case "arraycopy" =>
            Some(PyApplyExternal(PyExternalName("_scpy_arraycopy"), args)(PyVoidType, pos))
          case "identityHashCode" =>
            Some(PyApplyExternal(PyExternalName("_scpy_identity_hash_code"), args)(PyIntType, pos))
          case _ =>
            None
      else None

    /** True when `sym` is a non-static instance method whose receiver is
     *  a `java.lang.String`. The runtime String is a Python `str`, so the
     *  encoded Scala method name (`length__I`, etc.) does not resolve
     *  against it and we route to Python-native equivalents in
     *  `genStringCall`.
     */
    def isStringInstanceMethod(sym: Symbol, isStaticTarget: Boolean): Boolean =
      !isStaticTarget && sym.exists && sym.owner == defn.StringClass

    /** True when `sym` is a non-static instance method declared on the
     *  `java.lang.CharSequence` interface (`length`, `charAt`,
     *  `subSequence`, `toString`, `isEmpty`). The receiver's runtime type
     *  may be a Python `str` (when the static type was widened to
     *  `CharSequence`), in which case the encoded Scala method name does
     *  not resolve. We route through polymorphic `_scpy_charseq_*` helpers
     *  in `genCharSequenceCall`, which fall through to the encoded method
     *  on non-`str` receivers like `StringBuilder` / `ArrayCharSequence`.
     */
    def isCharSequenceInstanceMethod(sym: Symbol, isStaticTarget: Boolean): Boolean =
      !isStaticTarget && sym.exists && sym.owner == charSequenceClass

    /** True when `sym` is a non-static instance method on a JVM boxed-
     *  primitive class (`java.lang.Double`/`Float`/`Long`/`Integer`/
     *  `Byte`/`Short`/`Boolean`/`Number`) whose runtime receiver is
     *  almost certainly a raw Python `float`/`int`/`bool`. Boxing on
     *  this backend is identity (`boxToDouble(d)` returns the same
     *  Python `float`), so calls like `(d: java.lang.Double).isNaN()`
     *  end up dispatching against a `float`, which does not carry the
     *  encoded `isNaN__Z` method. Routed through `_scpy_Double_*` /
     *  `_scpy_Boolean_*` runtime helpers in `genBoxedPrimitiveCall`.
     *
     *  We intercept on the boxed-primitive owner rather than on
     *  `Object`/`AnyRef` because:
     *
     *  - `Object`-shape calls have already been routed by
     *    `genHashCodeSpecial` / `genGetClassSpecial` / `genToStringSpecial`
     *    (the only interesting `Object` instance methods on a primitive).
     *  - Calls whose static receiver is `java.lang.Double` etc. are
     *    typically introduced by Predef boxing implicits (`double2Double`)
     *    or explicit `asInstanceOf[java.lang.Double]` casts; the
     *    post-erasure `sym.owner` is the boxed-primitive class.
     *  - `java.lang.Number` is the abstract parent, so calls dispatched
     *    through a `Number`-typed value (e.g. generic `Numeric` code) also
     *    get caught here.
     */
    def isBoxedPrimitiveInstanceMethod(sym: Symbol, isStaticTarget: Boolean): Boolean =
      !isStaticTarget && sym.exists && {
        val owner = sym.owner
        owner == defn.BoxedDoubleClass ||
          owner == defn.BoxedFloatClass ||
          owner == defn.BoxedLongClass ||
          owner == defn.BoxedIntClass ||
          owner == defn.BoxedByteClass ||
          owner == defn.BoxedShortClass ||
          owner == defn.BoxedBooleanClass ||
          owner == defn.BoxedNumberClass
      }

    /** True when `sym` is a static call on `java.lang.String` (covers both
     *  the Java class and the synthetic linked module class, since
     *  `String.valueOf` etc. can be referenced through either depending on
     *  source shape).
     */
    def isStringStaticMethod(sym: Symbol, isStaticTarget: Boolean): Boolean =
      isStaticTarget && sym.exists &&
        (sym.owner == defn.StringClass || sym.owner == defn.StringModule)

    /** True when `classSym` is `java.lang.String` — used by `genApplyNew`
     *  to redirect String constructors to the synthetic `_String_`
     *  companion module.
     */
    def isStringClass(classSym: Symbol): Boolean =
      classSym.exists && classSym == defn.StringClass
  end Intrinsics

  // --- Scoped state --------------------------------------------------

  private var currentClassSym: Symbol = NoSymbol
  private var currentMethodSym: Symbol = NoSymbol

  /** Synthetic `@JavaStatic` helpers (anonfuns and HoistSuperArgs
    * `$superArg$N` methods) on a module class whose lifted body still
    * references the enclosing module's `this`. Populated by
    * `genClassMembers` *before* any method body is generated, so that
    * both the method definition (`genMethod`) and every call site
    * (`genNormalApply`, `genClosure`) see the same static-vs-instance
    * decision. See `needsSelfDespiteStatic` and
    * `notes/issue-anonfun-static-self-unbound.md`. */
  private val anonfunDemotedToInstance = mutable.Set.empty[Symbol]

  /** True when `sym` is one of the synthetic helpers we demote from
    * `@staticmethod` to a regular instance method.  Centralised so that
    * the demotion check is identical at the def site and at every call
    * site. */
  private def isAnonfunDemotedFromStatic(sym: Symbol): Boolean =
    sym.exists && anonfunDemotedToInstance.contains(sym)

  /** True when `sym` should be emitted / dispatched as a Python
    * `@staticmethod`. Mirrors the JVM/Scala.js backends'
    * `isStaticMember` check (`JavaStatic` ∪ `isScalaStatic`).
    *
    * `MoveStatics` lifts `@scala.annotation.static` methods from a
    * module class onto its companion class but intentionally does NOT
    * set `JavaStatic` on the lifted symbol (to preserve
    * `.enclosingClass`). Gating on `JavaStatic` alone therefore left
    * `@static`-annotated members emitted as instance methods on the
    * companion while every call site still routed through the module
    * receiver — surfacing at runtime as e.g.
    * `AttributeError: 'Test_' object has no attribute 'square__I__I'`.
    *
    * This predicate is the SINGLE source of truth for the decision.
    * Every static-vs-instance branch (decl namespace, call-site
    * receiver, intrinsic dispatch, `toString`/`getClass` exclusions,
    * closure target shape) MUST go through here so the two ends of the
    * contract stay consistent by construction. */
  private def isStaticMember(sym: Symbol): Boolean =
    sym.exists && (sym.is(JavaStatic) || sym.isScalaStatic)

  /** True when `sym` should be emitted / accessed as a class-level
    * (`@scala.annotation.static`) FIELD slot on its owner.
    *
    * Methods can use the broader `isStaticMember` predicate because
    * `PyApplyStatic` carries a built-in module-routing fallback at
    * emit time (a synthesized static-method forwarder on the
    * companion class delegates to the underlying module instance —
    * see `genStaticForwarders` and the `PyApplyStatic` emit cases).
    * Fields have no analogous indirection: the static-field forwarder
    * synthesized by `genStaticFieldForwarders` just sets a default
    * `None` at class level, so emitting `<Companion>.<field>` for a
    * JDK-typed `JavaStatic` access (e.g. `java.lang.Byte.TYPE`)
    * would read the uninitialized class slot instead of the module's
    * real storage. Restrict the field-side `PySelectStatic` routing
    * to genuinely Scala-static fields (`@scala.annotation.static`,
    * preserved across `MoveStatics`); JDK static fields fall through
    * to the existing module-routing path, where pylib stores the
    * value on the `_`-suffixed companion module. */
  private def isStaticFieldOwner(sym: Symbol): Boolean =
    sym.exists && sym.isScalaStatic

  /** True when `lhs` is an assignment to a per-lazy-val
    * `<container>$lzyHandle` VarHandle field.
    *
    * `LazyVals.scala` synthesizes one such field per lazy val and
    * marks the symbol with both `@ScalaStaticAnnot` and the
    * `LazyVarHandleName` name kind (see `LazyVals#transformValDef`
    * around `defn.MethodHandlesClass.select(MethodHandles_lookup)`).
    * `MoveStatics` then folds the field into the companion class's
    * `<clinit>` body. The emitter handles handle creation via the
    * runtime helper `_scpy_make_lazy_handle`, so the lifted
    * assignment must be dropped at codegen — see the call site in
    * `genStat`'s `Assign` arm. */
  private def isLazyVarHandleAssign(lhs: Tree): Boolean = lhs match
    case t: RefTree => t.symbol.exists && t.symbol.name.is(LazyVarHandleName)
    case _          => false

  /** Side-channel for statements produced during expression generation
    * (Block-in-expression-position). Drained by `flattenToStmts` at the
    * enclosing statement.
    *
    * INVARIANT: Mutated only by leaf emit sites (`+=` from `genExpr`'s
    * `Block`/`Return`/`Assign`/`WhileDo` arms, `genTryExpr`,
    * `genLabeledExpr`, `genSynchronizedExpr`, `hoistValueIf`,
    * `hoistValueMatch`, the array-set hoist) and the
    * `flattenToStmts` drain. Every scope boundary (where
    * pendings accumulated under a sub-tree must NOT escape into
    * the enclosing scope) MUST go through `withLocalDefScope`. Do
    * not add new ad-hoc save/replace/restore patterns. */
  private var pendingLocalDefs = mutable.ListBuffer.empty[PyTree]

  /** Run `body` against a fresh `pendingLocalDefs` buffer and return
    * `(prefixStatements, bodyResult)`. Restores the previously-installed
    * buffer regardless of how `body` exits.
    *
    * Replaces the older save/replace/restore idiom across the backend so
    * that side-channel pendings produced inside `body` are surfaced as
    * a structured prefix (which the caller splices at the right point)
    * instead of leaking into the enclosing scope. */
  private inline def withLocalDefScope[A](body: => A): (List[PyTree], A) =
    val saved = pendingLocalDefs
    pendingLocalDefs = mutable.ListBuffer.empty[PyTree]
    try
      val result = body
      (pendingLocalDefs.toList, result)
    finally
      pendingLocalDefs = saved

  // --- Entry point ---------------------------------------------------

  /** Per-unit driver that stops at `.pyir` emission and hands the
   *  in-memory class set to the GenPython phase for deferred linking.
   *  Returns `None` when `-scpy-ir-only` is set or no classes were
   *  emitted: there's nothing for the linker to do.
   *
   *  Driven by `GenPython.run`, which is invoked once per CU. The
   *  returned `PendingLink` is queued in the phase and drained after
   *  every CU has written its `.pyir`, so the per-CU link step sees
   *  every sibling CU on the classpath. See `GenPython.runOn`.
   */
  def runWriteOnly(): Option[GenPython.PendingLink] =
    pyDefn.force()
    genCompilationUnit(genCtx.compilationUnit)
    // REPL mode: a `PyReplSink` on the Context short-circuits the disk
    // write + link queue. The REPL driver runs its own link + emit cycle
    // against the cached startup support inputs, so producing `.pyir`
    // files in the (likely-shared) output directory would just be noise.
    genCtx.property(GenPython.ReplSinkKey) match
      case Some(sink) =>
        sink.onCompiled(sourceNameOfCu, generatedClasses.toList, mainEntry)
        None
      case None =>
        writeIRFile() match
          case Some(irFile) if !genCtx.settings.scpyIrOnly.value =>
            Some(new GenPython.PendingLink(
              sourceName       = sourceNameOfCu,
              sourceFile       = genCtx.compilationUnit.source,
              outputDirectory  = genCtx.settings.outputDir.value,
              irFile           = irFile,
              generatedClasses = generatedClasses.toList,
              mainEntry        = mainEntry
            ))
          case _ => None

  private def sourceNameOfCu: String =
    genCtx.compilationUnit.source.file.name.stripSuffix(".scala")

  /** Serialize `generatedClasses` to a `.pyir` in the output dir. Returns
   *  the written file (or `None` if nothing was emitted). */
  private def writeIRFile(): Option[dotty.tools.io.AbstractFile] =
    val outputDirectory = genCtx.settings.outputDir.value
    val sourceName = sourceNameOfCu
    val irFileName = deriveIrFileName(sourceName)
    val irFile = outputDirectory.fileNamed(irFileName)
    val irOut  = irFile.bufferedOutput
    try PyIRSerializer.serialize(generatedClasses.toList, mainEntry, irOut)
    finally irOut.close()
    Some(irFile)

  // --- Compilation unit traversal ------------------------------------

  private def genCompilationUnit(cunit: CompilationUnit): Unit =
    def collectTypeDefs(tree: Tree): List[TypeDef] = tree match
      case EmptyTree            => Nil
      case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
      case cd: TypeDef          => cd :: Nil
      case _: ValDef            => Nil
      case _                    => Nil

    val allTypeDefs = collectTypeDefs(cunit.tpdTree)

    // Track (sym, classDef) pairs so the forwarder post-pass can map a
    // module-class PyClassDef back to its originating ClassSymbol.
    val emitted = mutable.ListBuffer.empty[(ClassSymbol, PyClassDef)]

    // Collect every `@main`-bearing class encountered in this CU so we can
    // pick the bundle's main entry deterministically below. The previous
    // implementation overwrote `mainEntry` on every match, making the
    // selection sensitive to the order in which `allTypeDefs` arrived from
    // the pipeline (which is not guaranteed to be stable across runs).
    val mainCandidates = mutable.ListBuffer.empty[PyIREmitter.MainEntry]

    // CU-wide demotion pre-pass: walk every class's members BEFORE any
    // class's body codegen so the `anonfunDemotedToInstance` set is fully
    // populated when the FIRST class is generated. Otherwise the order of
    // `allTypeDefs` (which can shift under `-Ycheck:all` and other phase
    // re-typings) determines whether a sibling class's call site sees the
    // demotion. A SAM anon class generated before its enclosing module
    // class would emit `PyApplyStatic(...)` against a method that the
    // module class later emits as instance — the linker rejects with
    // `Unresolved static method`. See `tests/run/Parser.scala`.
    for td <- allTypeDefs do
      val sym = td.symbol
      if !encoding.hasExternAnnotation(sym)
         && !sym.isPrimitiveValueClass && sym != defn.ArrayClass
      then
        for tree <- collectMemberDefs(td) do
          tree match
            case dd: DefDef if !dd.symbol.isClassConstructor =>
              if needsSelfDespiteStatic(dd) then
                anonfunDemotedToInstance += dd.symbol
            case _ => ()

    for td <- allTypeDefs do
      val sym = td.symbol
      if encoding.hasExternAnnotation(sym) then
        // Force the binding read even if no call site reaches this facade -
        // otherwise malformed `@extern` args go unreported when the class
        // is never referenced. `externBindingOf` reports the diagnostic
        // through a dedupe set so repeated calls are safe.
        encoding.externBindingOf(sym)
        validateFacadeMemberNames(td)
      else if !sym.isPrimitiveValueClass && sym != defn.ArrayClass then
        currentClassSym = sym
        val kind =
          if sym.is(Trait) then PyClassKind.Interface
          else if isStaticModule(sym) then PyClassKind.ModuleClass
          else PyClassKind.Class
        val classDef = genClassDef(td, kind)
        emitted += ((sym.asClass, classDef))
        if genCtx.platform.hasMainMethod(sym) then
          mainCandidates += ((classDef.name, kind))

    mainEntry = PyCodeGenSupport.pickMainEntry(
      mainCandidates.toList,
      explicitName = genCtx.settings.XmainClass.value,
      onAmbiguity = (names, chosen) =>
        report.warning(
          s"ScalaPy: compilation unit ${cunit.source.file.name} " +
            s"declares multiple @main entry points (${names.mkString(", ")}); " +
            s"emitting `$chosen` as the bundle's main. " +
            "Set `-Xmain-class <name>` to choose explicitly.",
          NoSourcePosition
        )
    )

    emitWithStaticForwarders(emitted.toList)

  /** Append `emitted` classes to `generatedClasses`, weaving in static
   *  forwarders for top-level Scala `object`s.
   *
   *  Mirrors Scala.js's static-forwarder mechanism (see
   *  `inbox/scala-js/.../GenJSCode.scala:723-759, 1253-1308`). Stdlib code
   *  typechecked against the JVM JDK references `java.util.Arrays.copyOf`
   *  as a static method on the bare `java.util.Arrays` class, but our
   *  pylib defines `object Arrays` which encodes to `java.util.Arrays_`.
   *  Synthesizing a forwarder class named `java.util.Arrays` whose static
   *  methods delegate to the module bridges the two encodings without
   *  changing either side.
   *
   *  When the module has a companion class in this CU
   *  (`object Integer` + `class Integer`), forwarders are merged into the
   *  companion class rather than emitting a duplicate. */
  private def emitWithStaticForwarders(emitted: List[(ClassSymbol, PyClassDef)]): Unit =
    // Map each candidate module class to (target ownerName, method + field forwarders).
    // Target ownerName is the companion class name when one exists, else
    // the module-class name with the trailing `_` stripped.
    case class Plan(
        ownerName:  PyClassName,
        methods:    List[PyMethodDef],
        fields:     List[PyFieldDef],
        pos:        PyPosition
    )
    val planByModule = mutable.LinkedHashMap.empty[PyClassName, Plan]

    val byName: Map[PyClassName, PyClassDef] =
      emitted.iterator.map { case (_, cd) => cd.name -> cd }.toMap

    for (sym, classDef) <- emitted do
      // Static forwarders presuppose a singleton-bound module — without
      // it, the synthesized `_scpy_module_value(_scpy_mod_<name>_).foo()`
      // body references an unbound name. Inner module classes that
      // require an `_outer` ctor argument (e.g. `object Branch` in a
      // trait) survive the `Flatten` phase as `owner.is(Package)`-true
      // module classes, so `isForwarderCandidate` would otherwise admit
      // them; gate explicitly on the same singleton-eligibility
      // predicate the emitter uses for `_scpy_mod_*_` binding emission.
      if classDef.kind == PyClassKind.ModuleClass
        && isForwarderCandidate(sym)
        && PyIREmitter.hasModuleSingleton(classDef)
      then
        val ownerName = forwarderTargetName(sym, classDef)
        val companion = byName.get(ownerName)
        val existingMethodNames =
          companion.map(_.methods.map(_.name).toSet).getOrElse(Set.empty[PyMethodName])
        val existingFieldNames =
          companion.map(_.fields.map(_.name).toSet).getOrElse(Set.empty[PyFieldName])
        val methodFwds = genStaticForwarders(classDef, existingMethodNames)
        val fieldFwds  = genStaticFieldForwarders(classDef, ownerName, existingFieldNames)
        if methodFwds.nonEmpty || fieldFwds.nonEmpty then
          planByModule(classDef.name) = Plan(ownerName, methodFwds, fieldFwds, classDef.pos)

    // Index plans by their target ownerName so we can fold forwarders into
    // an emitted companion class on first encounter.
    val planByOwner: Map[PyClassName, Plan] = planByModule.values.iterator.map(p => p.ownerName -> p).toMap
    val foldedOwners = mutable.HashSet.empty[PyClassName]

    for (sym, classDef) <- emitted do
      planByOwner.get(classDef.name) match
        case Some(plan) if !foldedOwners.contains(classDef.name) =>
          // Companion class entry — append method + field forwarders.
          generatedClasses += classDef.copy(
            fields  = classDef.fields  ::: plan.fields,
            methods = classDef.methods ::: plan.methods
          )
          foldedOwners += classDef.name
        case _ =>
          generatedClasses += classDef

      // After a module class with no in-CU companion, emit a synthetic
      // forwarder class right after it so the final `.pyir` keeps the
      // forwarder beside its module.
      //
      // The `!byName.contains(plan.ownerName)` guard is load-bearing for
      // iteration-order independence: when the companion class IS in
      // this CU but happens to follow the module in `emitted`, the upper
      // match for the companion will fold the plan in via
      // `foldedOwners`. Emitting a synthetic forwarder here in that case
      // would produce a duplicate class with the companion's name (the
      // t6888 shape: `class abc$` + `object abc$` in the same CU,
      // encoded `abc_scpy_d` vs `abc__`). The plan owner is the
      // companion's encoded name, so checking `byName` is the precise
      // condition for "the upper match will handle this".
      if classDef.kind == PyClassKind.ModuleClass then
        planByModule.get(classDef.name) match
          case Some(plan)
              if !foldedOwners.contains(plan.ownerName)
                 && !byName.contains(plan.ownerName) =>
            generatedClasses += mkSyntheticForwarderClass(plan.ownerName, plan.fields, plan.methods, plan.pos)
            foldedOwners += plan.ownerName
          case _ => ()

  /** Top-level Scala `object`s only — matches scalac's JVM default for
   *  `BCodeHelpers.addForwarders`. Inner objects (e.g. private helpers
   *  nested in a top-level `object`) are not the source of stdlib link
   *  errors, and forwarding them risks unintended name collisions in the
   *  Python output. */
  private def isForwarderCandidate(sym: Symbol): Boolean =
    sym.is(ModuleClass) && !sym.isAnonymousClass && sym.owner.is(Package)

  private def forwarderTargetName(moduleSym: ClassSymbol, moduleClassDef: PyClassDef): PyClassName =
    val linked = moduleSym.linkedClass
    if linked.exists then encoding.encodeClassName(linked)
    else stripModuleSuffix(moduleClassDef.name)

  /** `java.util.Arrays_` → `java.util.Arrays`. Scala module classes
   *  encode with a trailing `_` (PyEncoding.sanitizeName turns the
   *  Scala `$` suffix into `_`). The Scala.js analogue is
   *  `nameString.stripSuffix("$")` at GenJSCode.scala:735. */
  private def stripModuleSuffix(name: PyClassName): PyClassName =
    val s = name.nameString
    if s.endsWith("_") then PyClassName(s.dropRight(1)) else name

  /** Synthetic forwarder class for a module that has no companion in this
   *  CU. Holds only `PublicStatic` fields and methods that delegate into
   *  the module. */
  private def mkSyntheticForwarderClass(
      name:    PyClassName,
      fields:  List[PyFieldDef],
      methods: List[PyMethodDef],
      pos:     PyPosition
  ): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = fields,
      methods      = methods,
      pos          = pos
    )

  /** Mirror of Scala.js's `genStaticForwardersFromModuleClass`
   *  (GenJSCode.scala:1253-1308): for each public instance method on the
   *  module, mint a `PublicStatic` forwarder whose body loads the module
   *  and dispatches the same method on it. Skips constructors, abstract
   *  members, private members, and any method whose signature already
   *  exists on the target owner (companion class case). */
  private def genStaticForwarders(
      moduleClassDef: PyClassDef,
      existingMethodNames: Set[PyMethodName]
  ): List[PyMethodDef] =
    val moduleName = moduleClassDef.name
    moduleClassDef.methods.flatMap { m =>
      val ns = m.flags.namespace
      if ns != PyMemberNamespace.Public then None
      else if m.body.isEmpty then None  // abstract — can't forward
      else if existingMethodNames.contains(m.name) then None  // already on owner
      else if isStaticForwarderProtocolMethod(m.name) then None
      else
        val paramRefs: List[PyTree] = m.args.map(p => PyVarRef(p.name)(p.ptpe, p.pos))
        val body = PyApply(
          flags     = PyApplyFlags.empty,
          dispatch  = PyDispatch.Virtual,
          receiver  = PyLoadModule(moduleName)(m.pos),
          className = moduleName,
          method    = m.name,
          args      = paramRefs
        )(m.resultType, m.pos)
        // Body is just the dispatch expression — `emitMethodDef` wraps the
        // last statement of a non-void method in `return` automatically
        // (see `PyIREmitter.emitMethodBody`).
        Some(PyMethodDef(
          flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
          name         = m.name,
          originalName = m.originalName,
          args         = m.args,
          resultType   = m.resultType,
          body         = Some(body),
          pos          = m.pos
        ))
    }

  private def isStaticForwarderProtocolMethod(name: PyMethodName): Boolean =
    val simple = name.simple.name
    isPythonDunderName(simple) ||
      (simple == "toString" && name.paramTypeRefs.isEmpty) ||
      (simple == "hashCode" && name.paramTypeRefs.isEmpty) ||
      (simple == "equals" && name.paramTypeRefs.length == 1)

  private def isPythonDunderName(name: String): Boolean =
    name.length >= 5 && name.startsWith("__") && name.endsWith("__")

  /** Static field forwarders for top-level module fields. Stdlib code
   *  typechecked against the JVM JDK sees primitives' `TYPE` as a static
   *  field on the bare class (e.g. `java.lang.Boolean.TYPE`), but pylib
   *  declares it on the Scala `object` (encoded `java.lang.Boolean_`).
   *  Replicating each module field as a `PublicStatic` field on the
   *  companion/synthetic class satisfies the JVM-style access pattern.
   *
   *  Crucially, `PyFieldName` carries an `owner` in its identity (unlike
   *  `PyMethodName`), so the new field name is rebuilt with the target
   *  ownerName — otherwise a stdlib `Select(Boolean, TYPE)` looking for
   *  `PyFieldName(java.lang.Boolean, TYPE)` wouldn't match the copied
   *  `PyFieldName(java.lang.Boolean_, TYPE)`. */
  private def genStaticFieldForwarders(
      moduleClassDef: PyClassDef,
      ownerName: PyClassName,
      existingFieldNames: Set[PyFieldName]
  ): List[PyFieldDef] =
    moduleClassDef.fields.flatMap { f =>
      val ns = f.flags.namespace
      if ns != PyMemberNamespace.Public then None
      else
        val newName = PyFieldName(ownerName, f.name.simple)
        if existingFieldNames.contains(newName) then None
        else
          Some(PyFieldDef(
            flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
            name         = newName,
            originalName = f.originalName,
            ftpe         = f.ftpe,
            pos          = f.pos
          ))
    }

  /** For each `@extern`-annotated class/object, check that no two members
   *  resolve to the same Python name. Python has no overloading, so
   *  colliding facade members would silently shadow each other on calls.
   *
   *  Only `DefDef`s are checked: every `val` generates a synthetic
   *  accessor `DefDef` that shares its name, so ValDefs would double-count
   *  if included. Call sites always resolve to the accessor, so the
   *  DefDef-only view matches runtime semantics. */
  private def validateFacadeMemberNames(td: TypeDef): Unit =
    val seen = mutable.Map.empty[String, Symbol]
    for tree <- collectMemberDefs(td) do tree match
      case dd: DefDef if !dd.symbol.isClassConstructor =>
        checkFacadeMember(dd.symbol, seen)
      case _ => ()

  private def checkFacadeMember(sym: Symbol, seen: mutable.Map[String, Symbol]): Unit =
    if encoding.externBindingOf(sym).isEmpty then
      val pyName = encoding.externMemberNameOf(sym)
      seen.get(pyName) match
        case Some(prior) =>
          report.error(
            s"""Facade member ${sym.show} resolves to Python name '$pyName',
               |which collides with ${prior.show} in the same facade.
               |Python has no overloading; use `@name` to disambiguate.""".stripMargin,
            sym.srcPos
          )
        case None =>
          seen(pyName) = sym

  // --- Class generation ----------------------------------------------

  private def genClassDef(td: TypeDef, kind: PyClassKind): PyClassDef =
    val sym = td.symbol.asClass
    val className = encoding.encodeClassName(sym)
    val (superClass, interfaces) = genBases(sym)
    val (fields, methodDefs) = genClassMembers(td)

    PyClassDef(
      name         = className,
      // Carry the JVM dotted full name so the runtime can reverse-map
      // `Class.getName()` back to JVM form (`Foo$$anon$1`) instead of the
      // Python-encoded form (`Foo__anon_1`). The emitter still extracts
      // the Scala simple name from this dotted form via
      // `stripScalaSimpleName`, which handles both inputs.
      originalName = PyOriginalName.fromString(encoding.jvmClassNameOf(sym)),
      kind         = kind,
      superClass   = superClass,
      interfaces   = interfaces,
      fields       = fields,
      methods      = methodDefs,
      pos          = posOf(td)
    )

  private def genBases(sym: ClassSymbol): (Option[PyClassSuper], List[PyClassName]) =
    val superSym = sym.superClass
    val superSlot: Option[PyClassSuper] =
      if superSym == defn.ObjectClass || superSym == NoSymbol then
        None
      else if encoding.hasExternAnnotation(superSym) then
        // Foreign Python parent. The user's facade carries the
        // module + class-path; emit an `Extern` superclass that
        // the linker and DCE leave alone and the emitter renders
        // through the extern-import alias machinery.
        encoding.externBindingOf(superSym) match
          case Some(binding) =>
            Some(PyClassSuper.Extern(binding.module, binding.path))
          case None =>
            // `externBindingOf` already reported a malformed-extern
            // diagnostic. Fall back to the nominal path so codegen
            // doesn't crash; the user sees the binding error.
            Some(PyClassSuper.Nominal(encoding.encodeClassName(superSym)))
      else
        Some(PyClassSuper.Nominal(encoding.encodeClassName(superSym)))
    val rawInterfaces = sym.directlyInheritedTraits
    for iface <- rawInterfaces if encoding.hasExternAnnotation(iface) do
      // v1 scope: only the single `extends` parent may be `@extern`.
      // An `@extern` trait in interface position would require
      // multiple-inheritance plumbing through the Python runtime that
      // we haven't built yet; reject with a clear message.
      report.error(
        s"""`@extern` trait '${iface.name.show}' cannot appear as a mix-in.
           |Only the single primary `extends` parent may target a Python class;
           |interface inheritance from `@extern` traits is not yet supported.""".stripMargin,
        sym.srcPos
      )
    val interfaceNames = rawInterfaces.map(encoding.encodeClassName)
    (superSlot, interfaceNames)

  // --- Class member collection ---------------------------------------

  private def genClassMembers(td: TypeDef): (List[PyFieldDef], List[PyMethodDef]) =
    val fields = mutable.ListBuffer.empty[PyFieldDef]
    val methods = mutable.ListBuffer.empty[PyMethodDef]

    // Pre-pass: identify synthetic `@JavaStatic` helpers whose body still
    // needs `self`. Must run before any method-body codegen so both the
    // def site and the call sites observe the same demotion decision.
    val members = collectMemberDefs(td)
    for tree <- members do
      tree match
        case dd: DefDef if !dd.symbol.isClassConstructor =>
          if needsSelfDespiteStatic(dd) then
            anonfunDemotedToInstance += dd.symbol
        case _ => ()

    for tree <- members do
      tree match
        case vd: ValDef =>
          val sym = vd.symbol
          if sym.is(Module) then ()
          else if encoding.hasExternAnnotation(sym) then
            // Proactive validation: force binding read so malformed args
            // are reported even if the val is never referenced.
            encoding.externBindingOf(sym)
          else
            // `@scala.annotation.static val/var` lifts the field from the
            // module class onto the companion class via `MoveStatics`,
            // intentionally without setting `JavaStatic` (so
            // `.enclosingClass` keeps pointing at the companion).
            // Detect via the field-side predicate `isStaticFieldOwner`
            // (= `isScalaStatic`) so the emitter knows to lay the field
            // out as a class-level attribute and skip the per-instance
            // zero-init in `__init__`. The same predicate gates
            // declaration and every read/write site, so the
            // static-vs-instance decision stays consistent by
            // construction. JDK static fields (`JavaStatic` only,
            // routed through `genStaticFieldForwarders`) keep their
            // existing module-routing read/write path and so do not
            // need the static-namespace flag here.
            val staticField = isStaticFieldOwner(sym)
            val baseFlags   = PyMemberFlags.empty.withMutable(sym.is(Mutable))
            val fieldFlags  =
              if staticField then baseFlags.withNamespace(PyMemberNamespace.PublicStatic)
              else baseFlags
            fields += PyFieldDef(
              flags        = fieldFlags,
              name         = encoding.encodeFieldName(sym),
              originalName = encoding.originalNameOf(sym),
              ftpe         = encoding.encodeType(sym.info),
              pos          = posOf(vd)
            )

        case dd: DefDef =>
          if dd.symbol.isClassConstructor then
            genConstructor(dd).foreach(methods += _)
          else if encoding.hasExternAnnotation(dd.symbol) then
            // Proactive validation: force binding read so malformed args
            // are reported even if the def is never called.
            encoding.externBindingOf(dd.symbol)
          else
            genMethod(dd).foreach(methods += _)

        case _ => ()

    (fields.toList, methods.toList)

  // --- Constructor generation ----------------------------------------

  private def genConstructor(dd: DefDef): Option[PyMethodDef] =
    val ctorSym = dd.symbol
    currentMethodSym = ctorSym
    val ctorPos = posOf(dd)
    encoding.withLocalScope {
      reserveModuleScopeIdentifiers(dd)
      val params = dd.termParamss.flatten.map(genParamDef)
      val body =
        if dd.rhs.isEmpty then Some(PySkip()(ctorPos))
        else Some(stmtsToBody(flattenToStmts(genStat(reorderCtorBody(dd.rhs))), ctorPos))

      Some(PyMethodDef(
        flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Constructor),
        name         = encoding.encodeMethodName(ctorSym),
        originalName = encoding.originalNameOf(ctorSym),
        args         = params,
        resultType   = PyVoidType,
        body         = body,
        pos          = ctorPos
      ))
    }

  /** dotc's `Constructors` phase emits the constructor body in
   *  `copyParams ::: super ::: lazyAssignments ::: stats` order. JVM
   *  bytecode requires `super` to come first; the JVM backend doesn't
   *  enforce that explicitly because JVM verification rejects field
   *  reads on `this` before `<init>` chains.
   *
   *  In Python every class shares one attribute slot per simple name
   *  per object, so two competing requirements exist:
   *
   *    (1) `class Child(override val msg: String) extends Parent(...)`
   *        — Parent's copyParams write `self.msg = parentMsg` during
   *        the super call. If Child's copyParams (`self.msg = msg`)
   *        ran *before* the super call, Parent would then clobber it.
   *        Subclass-wins semantics requires Child's copyParams to run
   *        *after* the super call.
   *
   *    (2) `tests/run/i763.scala` — Parent's primary ctor virtually
   *        invokes a child accessor (`self.s__I()`) that reads a field
   *        only set by the subclass. If Child's copyParams (`self.s = s`)
   *        ran only *after* the super call, Parent would observe the
   *        zero-init default and the assertion would fail. Child's
   *        copyParams must run *before* the super call.
   *
   *  Both (1) and (2) are satisfied by emitting the copyParams *twice*:
   *  once before the super-or-this-ctor call (so Parent's body sees the
   *  child-set values via virtual dispatch) and once after (so Parent's
   *  copyParams can't overwrite the child-set values). The duplicated
   *  block consists only of pure `Assign(ref(target), ref(param))`
   *  trees produced by `Constructors`, plus an optional null-test for
   *  the OUTER param; those are idempotent, so re-running them is safe.
   *
   *  The super call is always an `Apply` of a `Select` whose qualifier
   *  is `Super`; mix/this-init calls (`this(...)`) also qualify
   *  via the same shape but with a `This` qualifier and a constructor
   *  symbol — those follow the same JVM rule. */
  private def reorderCtorBody(rhs: Tree): Tree = rhs match
    case Block(stats, expr) =>
      val (preSuperCalls, rest) = stats.span(s => !isSuperOrThisCtorCall(s))
      rest match
        case ctorCall :: tail =>
          // Duplicate the copyParams: emit them once before the super
          // call (so the parent's virtual dispatch sees subclass-set
          // values) and once after (so the parent's copyParams cannot
          // clobber the subclass's overriding values). See class doc.
          val newStats = preSuperCalls ::: (ctorCall :: preSuperCalls) ::: tail
          Block(newStats, expr).withSpan(rhs.span)
        case Nil              => rhs
    case _ => rhs

  private def isSuperOrThisCtorCall(stat: Tree): Boolean = stat match
    case Apply(Select(_: Super, name), _) => name == nme.CONSTRUCTOR
    case Apply(Select(_: This,  name), _) => name == nme.CONSTRUCTOR
    case _ => false

  private def defaultValueFor(tpe: PyType, pos: PyPosition): PyTree = tpe match
    case PyBooleanType => PyBooleanLit(false)(pos)
    case PyByteType    => PyByteLit(0)(pos)
    case PyShortType   => PyShortLit(0)(pos)
    case PyCharType    => PyCharLit('\u0000')(pos)
    case PyIntType     => PyIntLit(0)(pos)
    case PyLongType    => PyLongLit(0L)(pos)
    case PyFloatType   => PyFloatLit(0.0f)(pos)
    case PyDoubleType  => PyDoubleLit(0.0)(pos)
    case _             => PyNullLit()(pos)

  // --- Method generation ---------------------------------------------

  private def genMethod(dd: DefDef): Option[PyMethodDef] = boundary:
    val sym = dd.symbol

    // Skip primitives. Bridge methods are kept: erased Closures
    // sometimes point at a synthetic `$anonfun$adapted$N` bridge that
    // adapts boxed `Object` args to a specialised primitive lambda body.
    if primitives.isPrimitive(sym) then break(None)

    currentMethodSym = sym
    val pos = posOf(dd)

    encoding.withLocalScope {
      reserveModuleScopeIdentifiers(dd)
      val params = dd.termParamss.flatten.map(genParamDef)
      val resultType = encoding.encodeType(sym.info.finalResultType)

      // A Scala `object` is a singleton instance, not a true static namespace.
      // Keep module-class methods as instance methods in PyIR so inherited
      // trait defaults can use normal receiver semantics (`super()`, `self`, ...).
      //
      // Anonfun-self override: dotc's erasure marks lifted lambdas /
      // hoistSuperArgs helpers on a module class as `@JavaStatic`, but
      // their lifted bodies can still reference the captured outer
      // receiver (e.g. an eta-expanded extension method `4.add` becomes
      // `def newFunction$$anonfun$1(y) = this.add(4, y)`). Emitting that
      // as a Python `@staticmethod` drops `self` from the parameter list
      // while the body still says `self.add(...)`, raising
      // `NameError: name 'self' is not defined` at runtime. The pre-pass
      // discriminator (`needsSelfDespiteStatic`, populated into
      // `anonfunDemotedToInstance`) detects exactly the bodies that need
      // the receiver and forces the namespace back to instance. The
      // matching predicate fires at every call site, so dispatch shape
      // stays consistent.  See `notes/issue-anonfun-static-self-unbound.md`.
      val isStatic = isStaticMember(sym) && !isAnonfunDemotedFromStatic(sym)
      val namespace = (isStatic, sym.is(Private)) match
        case (true,  true)  => PyMemberNamespace.PrivateStatic
        case (true,  false) => PyMemberNamespace.PublicStatic
        case (false, true)  => PyMemberNamespace.Private
        case (false, false) => PyMemberNamespace.Public

      val body: Option[PyTree] =
        if sym.is(Deferred) then None
        else if dd.rhs.isEmpty then Some(PySkip()(pos))
        else Some(stmtsToBody(flattenToStmts(genStat(dd.rhs)), pos))

      Some(PyMethodDef(
        flags        = PyMemberFlags.empty.withNamespace(namespace),
        name         = encoding.encodeMethodName(sym),
        originalName = encoding.originalNameOf(sym),
        args         = params,
        resultType   = resultType,
        body         = body,
        pos          = pos
      ))
    }

  private def genParamDef(p: ValDef): PyParamDef =
    val sym = p.symbol
    PyParamDef(
      name         = encoding.encodeLocalName(sym),
      originalName = encoding.originalNameOf(sym),
      ptpe         = encoding.encodeType(sym.info),
      mutable      = sym.is(Mutable),
      pos          = posOf(p)
    )

  /** Pre-pass: walk the method body and reserve, in the active local
   *  scope, the Python identifier of every top-level class the body
   *  reads at module scope.
   *
   *  Python's function-scope rule is that any local assignment to name
   *  `X` makes every other reference to `X` in the same `def` resolve
   *  to the local — the module-scope binding is shadowed for the whole
   *  function. Without reservation, a Scala source like
   *
   *      class opq
   *      val opq = new opq()
   *
   *  encodes the local symbol `opq` to identifier `"opq"`, the same
   *  string the emitter uses for `class opq:` at module scope. The
   *  generated Python (`opq = _scpy_new(opq, opq._scpy_ctor_...)`)
   *  reads `opq` on the RHS as the not-yet-bound local and raises
   *  `UnboundLocalError`. Reserving `"opq"` in the local scope BEFORE
   *  the param/local encoder runs reroutes the user's local through
   *  `freshUnique`, which picks `"opq_2"` and leaves the module-scope
   *  class binding readable.
   *
   *  Walks the parameter declarations and `rhs`. For every node that
   *  the codegen below renders as a bare class identifier in module
   *  scope (`New`, `This`, type tests, `Apply` on a static class
   *  receiver, `Ident` of a non-method/non-module class member, …) we
   *  collect the class symbol and reserve its encoded identifier.
   *
   *  Idempotent against the codegen below: the codegen MAY also lower
   *  references through `_scpy_*`-prefixed helpers (e.g. `PyLoadModule`
   *  uses `_scpy_mod_*_`, which never collides with user locals). For
   *  those we still reserve the unprefixed identifier; the reservation
   *  is just a string in `usedLocals`, and a stray reservation only
   *  costs one suffix bump on a colliding local. */
  private def reserveModuleScopeIdentifiers(dd: DefDef): Unit =
    val seenSyms = mutable.Set.empty[Symbol]
    def reserveClassSym(classSym: Symbol): Unit =
      if classSym.exists && classSym.isClass && !classSym.is(Package)
          && seenSyms.add(classSym) then
        // Skip facades (no Python class is emitted for them) and the
        // package class (its identifier is referenced via `_scpy_mod_*_`
        // module-vars only, which start with `_scpy_` and cannot collide).
        if !encoding.isFacadeSymbol(classSym) then
          val cn = encoding.encodeClassName(classSym)
          encoding.reserveLocalName(encoding.classIdentifierOf(cn))

    val walker = new TreeTraverser:
      override def traverse(tree: Tree)(using Context): Unit = tree match
        case _: New =>
          reserveClassSym(tree.tpe.typeSymbol)
          traverseChildren(tree)
        case t: This =>
          reserveClassSym(t.symbol)
          traverseChildren(tree)
        case TypeApply(_, targs) =>
          targs.foreach { ta => reserveClassSym(ta.tpe.typeSymbol) }
          traverseChildren(tree)
        case t @ Typed(_, tpt) =>
          reserveClassSym(tpt.tpe.typeSymbol)
          traverseChildren(tree)
        case id: Ident =>
          // A bare Ident referencing a class member without `self.`
          // qualification renders as `<ClassId>.<member>(...)` (static
          // call) or via a class-typed receiver. Reserve the owning
          // class so its identifier doesn't collide with a same-named
          // local elsewhere in the body. Modules are excluded — they
          // route through `_scpy_mod_*_`, which is `_scpy_`-prefixed.
          val sym = id.symbol
          if sym.exists && !sym.is(Module) && !sym.is(Package) then
            val owner = sym.owner
            if owner.exists && owner.isClass then reserveClassSym(owner)
        case sel @ Select(qual, _) =>
          // `Test.foo()` where `Test` is a module renders via
          // `_scpy_mod_*_`. But a static method on a non-module class
          // (a `@JavaStatic` lifted method on a companion class) is
          // rendered as `<ClassId>.<method>(...)`. Reserving the owner
          // for any selected symbol is conservative-correct.
          val selSym = sel.symbol
          if selSym.exists then
            val symOwner = selSym.owner
            if symOwner.exists && symOwner.isClass then reserveClassSym(symOwner)
          traverseChildren(tree)
        case _ =>
          traverseChildren(tree)

    // Walk parameter type declarations + the body. We don't need to
    // walk param names (they go through `encodeLocalName` with the
    // already-seeded scope), only sub-trees that produce module-scope
    // reads.
    dd.termParamss.flatten.foreach(p => walker.traverse(p.tpt))
    if !dd.rhs.isEmpty then walker.traverse(dd.rhs)

  /** Returns true when `dd` is a `@JavaStatic` synthetic helper on a
    * module class whose body still references the enclosing module's
    * receiver.
    *
    * dotc's erasure flags helpers inside a module class with
    * `JavaStatic` because module-class methods are emitted as JVM
    * `static` forwarders. But the bodies produced by `LambdaLift` /
    * `HoistSuperArgs` can still reference the captured outer `this` —
    * typically a bare `Ident(member)` whose owner is the same module
    * class. `genExpr` lowers such a reference to `PyThis()` (via
    * `moduleReceiver(currentClassSym)`), which renders as Python
    * `self`. Emitting the method as `@staticmethod` then drops `self`
    * from the parameter list while the body still lexically says
    * `self.<member>(...)`, raising
    * `NameError: name 'self' is not defined` at runtime.
    *
    * The discriminator walks the body, ignoring nested `DefDef` /
    * `TypeDef` (each has its own `self` scope), and returns true on
    * the first node that would lower to `self`:
    *  - `This(t)` where `t.symbol == enclosing`.
    *  - `Ident(x)` whose owner is `enclosing` and which is not a local
    *    / parameter / module / package (i.e. would resolve to
    *    `self.x` in `genExpr`).
    *
    * The walker descends into `Closure(env, meth, tpt)` because the
    * `meth` reference is what controls whether the closure body
    * evaluates `self`. Restricted to anonymous functions and
    * `HoistSuperArgs` `$superArg$N` helpers on module classes — the
    * only known synthesis paths that produce a static method whose
    * body needs the receiver — so we never demote genuinely
    * captureless statics. See `notes/issue-anonfun-static-self-unbound.md`. */
  private def needsSelfDespiteStatic(dd: DefDef): Boolean = boundary:
    val sym = dd.symbol
    val isSynthetic =
      sym.isAnonymousFunction
        || sym.name.toString.contains("superArg$")
    if !isSynthetic then break(false)
    val enclosing = sym.owner
    if !enclosing.is(ModuleClass) then break(false)
    if dd.rhs.isEmpty then break(false)

    val paramSyms: Set[Symbol] = dd.termParamss.flatten.map(_.symbol).toSet
    var found = false

    val walker = new TreeTraverser:
      override def traverse(tree: Tree)(using Context): Unit = boundary:
        if found then break()
        tree match
          // Don't descend into nested method/class definitions — they
          // have their own `self` scope; their bodies can't make the
          // outer body need `self`.
          case _: DefDef | _: TypeDef => ()
          case Closure(_, _, _) =>
            // Closure(env, meth, tpt) — the meth reference is what
            // determines whether the runtime evaluation reaches into
            // `self`. env values were already evaluated in this
            // method's scope before we entered the closure, so they
            // also count toward the outer-body's needs.
            traverseChildren(tree)
          case t: This if t.symbol == enclosing =>
            found = true
          case id: Ident =>
            val isym = id.symbol
            if isym.exists
                && isym.owner == enclosing
                && !paramSyms.contains(isym)
                && !isym.is(Local)
                && !isym.is(Module)
                && !isym.is(Package)
            then
              found = true
            else
              traverseChildren(tree)
          case _ =>
            traverseChildren(tree)

    walker.traverse(dd.rhs)
    found

  private def stmtsToBody(stmts: List[PyTree], pos: PyPosition): PyTree =
    stmts match
      case Nil       => PySkip()(pos)
      case hd :: Nil => hd
      case _         => PyBlock(stmts.init, stmts.last)(pos)

  // --- Statement generation ------------------------------------------

  /** Wrap `doGenStat` so any `pendingLocalDefs` produced during this
   *  statement are drained here and emitted as a prefix of the returned
   *  tree. Otherwise pendings accumulate across sibling statements and
   *  all end up hoisted to the method root by `flattenToStmts`, which
   *  scrambles side-effect ordering (e.g. an assignment inside a
   *  `Block`-in-expression would execute before an earlier statement). */
  private def genStat(tree: Tree): PyTree =
    val (locals, result) = withLocalDefScope(doGenStat(tree))
    if locals.isEmpty then result
    else PyBlock(locals, result)(posOf(tree))

  private def doGenStat(tree: Tree): PyTree =
    val pos = posOf(tree)
    tree match
      case vd: ValDef =>
        val sym = vd.symbol
        val rhs = if vd.rhs.isEmpty then PyNullLit()(pos) else genExpr(vd.rhs)
        PyVarDef(
          name         = encoding.encodeLocalName(sym),
          originalName = encoding.originalNameOf(sym),
          vtpe         = encoding.encodeType(sym.info),
          mutable      = sym.is(Mutable),
          rhs          = rhs
        )(pos)

      case tree @ If(cond, thenp, elsep) =>
        if isValueProducingScalaType(tree.tpe) then genExpr(tree)
        else PyIf(genExpr(cond), genStat(thenp), genStat(elsep))(PyVoidType, pos)

      case t @ Labeled(bind, expr) =>
        // If the labeled block produces a value (non-Unit, non-Nothing
        // Scala type), route through `genLabeledExpr` so a temp var
        // captures the escape value and the caller can read it. Without
        // this, a `Labeled` reached from `genStat` (e.g. as an arm of
        // an `If` at method tail where `doGenStat(If)` processes arms
        // via `genStat`) would drop the match's result — the emitter's
        // `except _scpy_lbl_N: pass` clause discards the exception
        // value because no temp exists to capture it.
        val tpe = t.tpe
        if tpe.exists && !tpe.isRef(defn.UnitClass) && !tpe.isRef(defn.NothingClass) then
          genLabeledExpr(t)
        else
          PyLabeled(
            encoding.encodeLabelName(bind.symbol),
            genStat(expr)
          )(PyVoidType, pos)

      case tr @ Return(expr, from) =>
        // PatternMatcher wraps every match arm in `Return(matchLabel,
        // <arm body>)`. When an arm body is itself an unconditional
        // escape (e.g. `case _ => return 99` → `Return(matchLabel,
        // Return(methodSym, 99))`), the outer wrap is dead code — the
        // inner return jumps out before the outer one runs. Strip it
        // so we don't emit a dead `PyLabelReturn` and don't attempt to
        // `genExpr` a `Return` (unsupported in expression position).
        expr match
          case _: Return => doGenStat(expr)
          case _ =>
            val fromSym = from.symbol
            val value =
              if expr == EmptyTree then PyUnitLit()(pos)
              else genExpr(expr)
            if fromSym.is(Label) then
              PyLabelReturn(encoding.encodeLabelName(fromSym), value)(pos)
            else
              PyReturn(value)(pos)

      case WhileDo(cond, body) =>
        // The condition itself may lower to side-effecting statements +
        // a value (e.g. a hoisted `&&`/`||` short-circuit `if` that
        // reads a `.nn`-checked field). Those statements must run on
        // every iteration, not once before the loop, otherwise the
        // condition stays stale: `while cond` re-reads the temp
        // without re-running the locals that wrote into it. The bug
        // surfaced as a `null.parent` deref in the iterative tree-walk
        // in `mutable.RedBlackTree.successor`.
        if cond == EmptyTree then
          PyWhile(PyBooleanLit(true)(pos), genStat(body))(pos)
        else
          val (condLocals, condExpr) = genExprWithPending(cond)
          val genBody = genStat(body)
          if condLocals.isEmpty then
            PyWhile(condExpr, genBody)(pos)
          else
            // Use a flag variable so the cond locals are re-run each
            // iteration. Lower `while c { body }` to:
            //   var __keep = True
            //   while __keep:
            //     <condLocals>
            //     if not c: __keep = False
            //     else: <body>
            val flagName = freshLoopFlagName()
            val flagVar  = PyVarRef(flagName)(PyBooleanType, pos)
            val flagDef  = PyVarDef(
              name         = flagName,
              originalName = PyOriginalName.NoOriginalName,
              vtpe         = PyBooleanType,
              mutable      = true,
              rhs          = PyBooleanLit(true)(pos)
            )(pos)
            val checkIf = PyIf(
              condExpr,
              genBody,
              PyAssign(flagVar, PyBooleanLit(false)(pos))(pos)
            )(PyVoidType, pos)
            val loopBodyStmts = condLocals :+ checkIf
            val loop = PyWhile(flagVar, stmtsToBody(loopBodyStmts, pos))(pos)
            PyBlock(List(flagDef), loop)(pos)

      case t: Try =>
        genTry(t)

      case Assign(lhs, rhs) =>
        // Drop assignments to per-lazy-val `<container>$lzyHandle`
        // VarHandle fields that `MoveStatics` lifts into `<clinit>`.
        // The JVM body uses `MethodHandles.lookup().findVarHandle(...)`
        // to bind the handle; the Python backend never wires that
        // call (no real `MethodHandles` infrastructure) and instead
        // initializes each lazy-handle field at class-definition time
        // via the runtime helper `_scpy_make_lazy_handle(...)` (see
        // `PyIREmitter.classLevelFieldInitExpr`'s `_lzyHandle`-suffix
        // arm). Re-running the JVM-shaped assignment from `<clinit>`
        // would either NameError on the unbundled
        // `_scpy_mod_java_lang_invoke_MethodHandles_` proxy or
        // overwrite the runtime-shim handle with a broken object.
        // Skipping at codegen keeps the contract one-sided: the
        // emitter is the single source of truth for the handle's
        // initialization expression.
        if isLazyVarHandleAssign(lhs) then PySkip()(pos)
        else PyAssign(genAssignableLhs(lhs), genExpr(rhs))(pos)

      case Block(stats, expr) =>
        val statTrees = stats.map(genStat)
        val exprTree  = genStat(expr)
        PyBlock(statTrees, exprTree)(pos)

      case app: Apply if isSynchronizedPrimitive(app) =>
        genSynchronizedStat(app)

      case app: Apply =>
        genApply(app)

      case app: TypeApply =>
        genTypeApply(app)

      case EmptyTree =>
        PySkip()(pos)

      case _ =>
        // Fall through: treat as expression. The emitter emits any non-void
        // expression as a statement by printing its text on a line.
        genExpr(tree)

  /** Build a `PyAssignable` LHS for an assignment. Scala's typer
   *  guarantees `Assign.lhs` is a valid LHS tree, so the matched cases
   *  cover everything. */
  private def isValueProducingScalaType(tpe: Type): Boolean =
    tpe.exists &&
      !tpe.isRef(defn.UnitClass) &&
      !tpe.isRef(defn.BoxedUnitClass) &&
      !tpe.isRef(defn.NothingClass)

  private def genAssignableLhs(tree: Tree): PyAssignable =
    val pos = posOf(tree)
    tree match
      case id: Ident =>
        val sym = id.symbol
        // A bare `Ident` inside a class body can resolve to an
        // instance field (e.g. the `_outer` param-accessor) rather than
        // a local. Treat it as `this.field = rhs` so the generated
        // Python touches the right storage. We detect "instance field"
        // as: owner is a class + not a Method + not a static (Module
        // singleton) member. `Param` alone is not disqualifying — a
        // constructor param-accessor can keep both `Param` and `ParamAccessor`
        // flags while still representing a field on the class.
        if isStaticFieldOwner(sym) && !sym.is(Method) && !sym.is(Module)
            && !sym.is(Package)
        then
          // `@scala.annotation.static val/var`. The synthetic `<clinit>`
          // body emits `Assign(ref(field), rhs)` where `field`'s symbol
          // owner is the companion class. Route to a class-level
          // attribute write so the field lands on `<Companion>.<name>`,
          // matching the declaration site (which threaded the same
          // `isScalaStatic` check into the field's namespace).
          PySelectStatic(encoding.encodeFieldName(sym))(encoding.encodeType(tree.tpe), pos)
        else if sym.owner.is(ModuleClass) && !sym.is(Method) && !sym.is(Module)
            && !sym.is(Package)
        then
          PySelect(
            moduleReceiver(sym.owner, pos),
            encoding.encodeFieldName(sym)
          )(encoding.encodeType(tree.tpe), pos)
        else if sym.owner.isClass && !sym.is(Method) && !sym.is(Module)
            && !sym.is(Package)
        then
          val classTpe = PyClassType(encoding.encodeClassName(currentClassSym))
          PySelect(
            PyThis()(classTpe, pos),
            encoding.encodeFieldName(sym)
          )(encoding.encodeType(tree.tpe), pos)
        else
          PyVarRef(encoding.encodeLocalName(sym))(encoding.encodeType(tree.tpe), pos)
      case sel @ Select(qual, _) =>
        val sym = sel.symbol
        if isStaticFieldOwner(sym) && !sym.is(Method) && !sym.is(Module)
            && !sym.is(Package)
        then
          // `@scala.annotation.static val/var` write through a qualified
          // selection (e.g. `Foo.field = rhs`). Route to a class-level
          // attribute write on the lifted owner, matching the
          // declaration / read sites that consult the same predicate.
          // The qualifier is discarded because the field lives on the
          // class slot, not on the qualifier instance — Scala's typer
          // already proved the qualifier reduces to the static owner.
          PySelectStatic(encoding.encodeFieldName(sym))(encoding.encodeType(tree.tpe), pos)
        else
          PySelect(
            genExpr(qual),
            encoding.encodeFieldName(sym)
          )(encoding.encodeType(tree.tpe), pos)
      case _ =>
        report.error(s"Unsupported assignment LHS: ${tree.show}", tree.sourcePos)
        PyVarRef(PyLocalName("_scpy_error"))(PyAnyType, pos)

  // --- Expression generation -----------------------------------------

  private def genExpr(tree: Tree): PyTree =
    val pos = posOf(tree)
    tree match
      case Literal(value) =>
        genLiteral(value, pos)

      case seq: JavaSeqLiteral =>
        genArrayLiteral(seq.tpe, seq.elems, pos)

      case If(cond, thenp, elsep) =>
        // Each branch must scope its own `pendingLocalDefs` to prevent
        // side-effecting genExpr calls (notably `genExpr(Return)`) from
        // leaking into the enclosing scope and running unconditionally.
        //
        // INVARIANT: A `PyIf` returned from `genExpr` must not contain
        // a `PyBlock` (or any other statement-shaped node) in its
        // branches. The emitter would render such a `PyIf` as a Python
        // conditional expression `(thenp if cond else elsep)` and
        // silently drop the block's statements — see
        // `notes/issue-list-vector-large-literal-unbound-locals.md`.
        // When either branch has pending locals, hoist the whole `If`
        // to a statement-form temp assign + `PyVarRef` for the value.
        def scopedBranch(branch: Tree): (List[PyTree], PyTree) =
          withLocalDefScope(genExpr(branch))
        val condIR = genExpr(cond)
        val (thenLocals, thenExpr) = scopedBranch(thenp)
        val (elseLocals, elseExpr) = scopedBranch(elsep)
        if thenLocals.isEmpty && elseLocals.isEmpty then
          PyIf(condIR, thenExpr, elseExpr)(encoding.encodeType(tree.tpe), pos)
        else
          hoistValueIf(condIR, thenLocals, thenExpr, elseLocals, elseExpr,
                       encoding.encodeType(tree.tpe), pos)

      case t: This =>
        if t.symbol.is(ModuleClass) && t.symbol != currentClassSym then
          if t.symbol == defn.StringModule then PyLoadModule(stringCompanionClassName)(pos)
          else PyLoadModule(encoding.encodeClassName(t.symbol))(pos)
        else
          PyThis()(encoding.encodeType(tree.tpe), pos)

      case Select(qualifier, _) =>
        val sel = tree.asInstanceOf[Select]
        if encoding.isFacadeOwner(sel.symbol.owner) then
          genFacadeSelect(sel, pos)
        else
          val sym = tree.symbol
          if isStringStaticField(sym) then
            PySelect(
              PyLoadModule(stringCompanionClassName)(pos),
              PyFieldName(stringCompanionClassName, PySimpleFieldName(sym.name.mangledString))
            )(encoding.encodeType(tree.tpe), pos)
          else if isSystemStreamStaticFieldRef(sym) then
            // The qualifier is whatever resolved to `java.lang.System` —
            // for stdlib (typed against the JVM JDK) it's the class
            // symbol itself, not a Scala module, so the
            // `isStaticOwnerRef` guard would falsely reject it. The
            // owner+name check in `isSystemStreamStaticFieldRef` is
            // already specific enough that no user code can hit it.
            genStaticFieldGetter(sym, tree.tpe, pos)
          else if sym.is(Module) then
            if isStringCompanionModule(sym) then PyLoadModule(stringCompanionClassName)(pos)
            else if sym == pyDefn.EmptyTupleModule then PyTupleValue(Nil)(encoding.encodeType(tree.tpe), pos)
            else PyLoadModule(encoding.encodeClassName(sym.moduleClass))(pos)
          else if isStaticFieldOwner(sym) && !sym.is(Method) then
            // `@scala.annotation.static val/var` read through a
            // qualified selection (e.g. `Foo.field`). The lifted symbol
            // owner is the companion class; route to a class-level
            // attribute read, mirroring the declaration / write sites
            // that consult the same predicate. See item 11 (commit
            // `9aa829b83b`) for the method-side analogue.
            PySelectStatic(encoding.encodeFieldName(sym))(encoding.encodeType(tree.tpe), pos)
          else
            PySelect(
              genExpr(qualifier),
              encoding.encodeFieldName(sym)
            )(encoding.encodeType(tree.tpe), pos)

      case id: Ident =>
        val sym = id.symbol
        encoding.externBindingOf(sym) match
          case Some(binding) =>
            genExternRef(binding, tree.tpe, pos)
          case None =>
            if sym.is(Module) then
              if isStringCompanionModule(sym) then PyLoadModule(stringCompanionClassName)(pos)
              else if sym == pyDefn.EmptyTupleModule then PyTupleValue(Nil)(encoding.encodeType(tree.tpe), pos)
              else PyLoadModule(encoding.encodeClassName(sym.moduleClass))(pos)
            else if isSystemStreamStaticFieldRef(sym) then
              genStaticFieldGetter(sym, tree.tpe, pos)
            else if isStaticFieldOwner(sym) && !sym.is(Method) && !sym.is(Module)
                && !sym.is(Package)
            then
              // Bare-name read of a `@scala.annotation.static val/var`
              // (e.g. `field` inside the synthesized `<clinit>` body or
              // inside a sibling `@static def` that references another
              // `@static` field on the same companion class). Route to
              // the class-level attribute, matching the assign-LHS and
              // qualified-Select branches that consult the same
              // predicate.
              PySelectStatic(encoding.encodeFieldName(sym))(encoding.encodeType(tree.tpe), pos)
            else if sym.owner.is(ModuleClass) && !sym.is(Method) && !sym.is(Module)
                && !sym.is(Package)
            then
              PySelect(
                moduleReceiver(sym.owner, pos),
                encoding.encodeFieldName(sym)
              )(encoding.encodeType(tree.tpe), pos)
            else if sym.owner.isClass && !sym.is(Method) && !sym.is(Module)
                && !sym.is(Package)
            then
              // Bare-name read of an instance field — emit `self.field`.
              // Matches the assign-LHS branch in `genAssignableLhs`.
              val classTpe = PyClassType(encoding.encodeClassName(currentClassSym))
              PySelect(
                PyThis()(classTpe, pos),
                encoding.encodeFieldName(sym)
              )(encoding.encodeType(tree.tpe), pos)
            else
              PyVarRef(encoding.encodeLocalName(sym))(encoding.encodeType(tree.tpe), pos)

      case Block(stats, expr) =>
        // Side effects become pending local defs; return the final expr.
        for s <- stats do pendingLocalDefs += genStat(s)
        genExpr(expr)

      case Typed(sup: Super, _) =>
        PyThis()(encoding.encodeType(tree.tpe), pos)
      case Typed(inner, _) =>
        genExpr(inner)

      case app: Apply if isSynchronizedPrimitive(app) =>
        genSynchronizedExpr(app)

      case app: Apply =>
        genApply(app)

      case app: TypeApply =>
        genTypeApply(app)

      case tree: Closure =>
        genClosure(tree)

      case Match(selector, cases) =>
        genMatchExpr(selector, cases, pos, encoding.encodeType(tree.tpe))

      case t: Try =>
        genTryExpr(t)

      case t: Labeled =>
        genLabeledExpr(t)

      case tr: Return =>
        // `Return` in expression position (Nothing-typed): emit the
        // escape as a pending statement, return a dummy Unit. The
        // "expression value" is unreachable since `return` jumps out.
        pendingLocalDefs += doGenStat(tr)
        PyUnitLit()(pos)

      case _: (WhileDo | Assign) =>
        // `WhileDo` and `Assign` are Unit-typed and routinely appear in
        // expression position in stdlib code (typically as a case-arm
        // RHS, e.g. `case xs: Array[Int] => while (...) { ... }`). Emit
        // the side effect as a pending statement and supply a Unit
        // value in its place.
        pendingLocalDefs += doGenStat(tree)
        PyUnitLit()(pos)

      case EmptyTree =>
        PyUnitLit()(pos)

      case _ =>
        report.error(
          s"Python backend: unhandled expression form ${tree.getClass.getSimpleName}: ${tree.show}",
          tree.sourcePos
        )
        PyUnitLit()(pos)

  // --- Literal generation --------------------------------------------

  private def genLiteral(value: Constant, pos: PyPosition): PyTree =
    value.tag match
      case UnitTag    => PyUnitLit()(pos)
      case BooleanTag => PyBooleanLit(value.booleanValue)(pos)
      case ByteTag    => PyByteLit(value.byteValue)(pos)
      case ShortTag   => PyShortLit(value.shortValue)(pos)
      case CharTag    => PyCharLit(value.charValue)(pos)
      case IntTag     => PyIntLit(value.intValue)(pos)
      case LongTag    => PyLongLit(value.longValue)(pos)
      case FloatTag   => PyFloatLit(value.floatValue)(pos)
      case DoubleTag  => PyDoubleLit(value.doubleValue)(pos)
      case StringTag  => PyStringLit(value.stringValue)(pos)
      case NullTag    => PyNullLit()(pos)
      case ClazzTag   => PyClassOf(encoding.encodeTypeRef(value.typeValue))(pos)
      case _          =>
        // Unknown literal constant kind reaching the backend means a
        // post-erasure invariant is broken. Surface a real diagnostic
        // (so all bad literals in the CU are collected) and fall back to
        // a Unit value to keep compilation going.
        report.error(
          s"Python backend: unsupported literal constant kind ${value.tag} (value ${value.value})",
          sourcePosOf(pos)
        )
        PyUnitLit()(pos)

  // --- Apply dispatch ------------------------------------------------

  private def genApply(app: Apply): PyTree = boundary:
    val pos = posOf(app)

    // `throw <expr>` is encoded as `Apply(<special-ops>.throw, [expr])`
    // by `tpd.Throw`. The owner `<special-ops>` is a synthetic package
    // class with no Python representation, so we lower the call to a
    // PyIR Throw unary op.
    if app.fun.symbol == defn.throwMethod then
      break(PyUnaryOp(PyUnaryCode.Throw, genExpr(app.args.head))(pos))

    genArrayFactoryApply(app, pos) match
      case Some(arrayValue) =>
        break(arrayValue)
      case None =>
        ()

    if app.fun.symbol == defn.newArrayMethod then
      break(genRuntimeNewArray(app, pos))

    genReflectArrayNewInstance(app, pos) match
      case Some(tree) =>
        break(tree)
      case None =>
        ()

    app.fun match
      case id: Ident if encoding.externBindingOf(id.symbol).isDefined =>
        genExternCall(id.symbol, app.args, pos)

      // super.method(args)
      case Select(_: Super, _) =>
        genSuperCall(app, pos)

      // new ClassName(args)
      case Select(New(tpt), nme.CONSTRUCTOR) if encoding.externBindingOf(tpt.tpe.typeSymbol).isDefined =>
        genFacadeNew(app, pos)

      case Select(New(tpt), nme.CONSTRUCTOR) =>
        genApplyNew(app, pos)

      case sel @ Select(_, _) if sel.symbol.exists && encoding.isFacadeOwner(sel.symbol.owner) =>
        if sel.symbol.is(Accessor) && app.args.isEmpty then genFacadeSelect(sel, pos)
        else genFacadeCall(sel, app.args, pos)

      // `Select(qual, name)` where the selected member itself carries an
      // `@extern` binding but its owner is a regular Scala class/object
      // (not a `@extern`-annotated facade). The bare-name `Ident` arm above
      // covers same-scope calls like `pyPack(...)` from inside the same
      // `object`; this arm covers cross-class calls like
      // `ObjectOutputStream.pickleDumps(obj)` from inside the companion
      // class. In both cases the extern annotation says "this method's
      // body is `native` — emit a dynamic call to the bound module/path
      // and discard the receiver." `genExternCall` honours that contract;
      // the qualifier is intentionally not threaded into the call because
      // it is a regular Scala module receiver, not a Python facade
      // qualifier.
      case sel @ Select(_, _) if sel.symbol.exists && encoding.externBindingOf(sel.symbol).isDefined =>
        genExternCall(sel.symbol, app.args, pos)

      case _ =>
        val sym = app.fun.symbol
        if primitives.isPrimitive(app) then
          genPrimitiveOp(app, pos)
        else if Erasure.Boxing.isBox(sym) then
          genBoxIfChar(sym, genExpr(app.args.head), pos)
        else if Erasure.Boxing.isUnbox(sym) then
          genUnboxIfChar(sym, genExpr(app.args.head), pos)
        else if isBoxesRunTimeBoxToCharacter(sym) then
          PyApplyExternal(PyExternalName("_scpy_box_char"), List(genExpr(app.args.head)))(PyAnyType, pos)
        else if isBoxesRunTimeUnboxToChar(sym) then
          PyApplyExternal(PyExternalName("_scpy_unbox_char"), List(genExpr(app.args.head)))(PyCharType, pos)
        else
          genNormalApply(app, pos)

  /** Char is the only primitive whose boxed display form differs from
   *  its unboxed Python representation — `Char` is stored as a Python
   *  `int`, but `String.valueOf(boxedChar)` (and thus `_scpy_to_str`) must
   *  render the codepoint as the corresponding 1-character string.
   *  Wrap the value in `_scpy_Char` (an `int` subclass with a
   *  `toString__Ljava_lang_String` hook) so the boxed-and-widened path
   *  produces `"a"`, not `"97"`. Other primitives (Int, Long, etc.) round
   *  trip through `str(x)` correctly already, so their box is identity. */
  private def genBoxIfChar(sym: Symbol, arg: PyTree, pos: PyPosition): PyTree =
    if sym.owner.linkedClass == defn.CharClass then
      PyApplyExternal(PyExternalName("_scpy_box_char"), List(arg))(PyAnyType, pos)
    else
      arg

  /** Lower a primitive unbox call inserted by `Erasure.Boxing` (e.g.
   *  `BoxesRunTime.unboxToInt(x)`).
   *
   *  - `Char` round-trips through `_scpy_Char` (an `int` subclass with a
   *    `toString` hook); unbox extracts the raw codepoint and raises
   *    `NullPointerException` on `null` (matches `unboxToChar(null)`).
   *  - Other primitives (Z/B/S/I/J/F/D): JVM `BoxesRunTime.unboxTo*(null)`
   *    returns the primitive default rather than raising. Route through
   *    `_scpy_unbox_or_default(tag, value)` so erased generics like
   *    `def gen[A]: A = null.asInstanceOf[A]` followed by
   *    `val i: Int = gen[Int]` produce `0` instead of leaving `i = None`.
   *  - Non-primitive unbox owners: identity. */
  private def genUnboxIfChar(sym: Symbol, arg: PyTree, pos: PyPosition): PyTree =
    val linked = sym.owner.linkedClass
    if linked == defn.CharClass then
      PyApplyExternal(PyExternalName("_scpy_unbox_char"), List(arg))(PyCharType, pos)
    else
      primitiveTagFor(linked) match
        case Some((tag, tpe)) =>
          PyApplyExternal(
            PyExternalName("_scpy_unbox_or_default"),
            List(PyStringLit(tag)(pos), arg)
          )(tpe, pos)
        case None => arg

  private def primitiveTagFor(linked: Symbol): Option[(String, PyType)] =
    if      linked == defn.BooleanClass then Some(("Z", PyBooleanType))
    else if linked == defn.ByteClass    then Some(("B", PyByteType))
    else if linked == defn.ShortClass   then Some(("S", PyShortType))
    else if linked == defn.IntClass     then Some(("I", PyIntType))
    else if linked == defn.LongClass    then Some(("J", PyLongType))
    else if linked == defn.FloatClass   then Some(("F", PyFloatType))
    else if linked == defn.DoubleClass  then Some(("D", PyDoubleType))
    else None

  private def isBoxesRunTimeBoxToCharacter(sym: Symbol): Boolean =
    sym.exists && sym.owner == defn.BoxesRunTimeModule.moduleClass &&
      sym.name.mangledString == "boxToCharacter"

  private def isBoxesRunTimeUnboxToChar(sym: Symbol): Boolean =
    sym.exists && sym.owner == defn.BoxesRunTimeModule.moduleClass &&
      sym.name.mangledString == "unboxToChar"

  private def genSuperCall(app: Apply, pos: PyPosition): PyTree = boundary:
    val sym = app.fun.symbol
    val args = genArgsPreservingOrder(app.args)
    val tpe = encoding.encodeType(sym.info.finalResultType)
    val classTpe = PyClassType(encoding.encodeClassName(currentClassSym))
    // Foreign Python parent: route the super call through the same
    // dynamic-call machinery the rest of the facade infrastructure uses.
    // For the `__init__` chain we emit `<extern>.<__init__>(self, ...)`;
    // the bundled Python `class Foo(<extern>):` declaration carries the
    // MRO so attribute lookup for inherited fields still resolves.
    if encoding.hasExternAnnotation(sym.owner) && sym.isClassConstructor then
      encoding.externBindingOf(sym.owner) match
        case Some(binding) =>
          val initRef = PyExternalRef(
            binding.module,
            binding.path :+ "__init__"
          )(PyAnyType, pos)
          val self = PyThis()(classTpe, pos)
          break(PyApplyDynamic(initRef, self :: args, Nil)(tpe, pos))
        case None =>
          // Malformed extern — fall through to the nominal path; the
          // facade-binding error is already reported.
          ()
    val ownerName = encoding.encodeClassName(sym.owner)
    val methodName = encoding.encodeMethodName(sym)
    PyApply(
      PyApplyFlags.empty,
      PyDispatch.Static,
      PyThis()(classTpe, pos),
      ownerName,
      methodName,
      args
    )(tpe, pos)

  private def genApplyNew(app: Apply, pos: PyPosition): PyTree =
    val Apply(fun @ Select(New(tpt), _), args) = app: @unchecked
    val classSym = tpt.tpe.typeSymbol
    val pyArgs = genArgsPreservingOrder(args)
    if Intrinsics.isStringClass(classSym) then
      genStringCtorCall(fun.symbol, pyArgs, pos)
    else if isFixedArityTupleClass(classSym) then
      // `new Tuple{N}(...)` — lower to a native Python tuple value.
      // Restricted to fixed-arity tuple classes (`Tuple1..22` and
      // specialized forms): `new TupleXXL(arr)` and `new *:(h, t)`
      // need their real Scala constructors so the runtime keeps the
      // expected class identity for downstream `unapplySeq`/cons-shape
      // operations.
      PyTupleValue(pyArgs)(encoding.encodeType(app.tpe), pos)
    else
      val className = encoding.encodeClassName(classSym)
      val ctorName = encoding.encodeMethodName(fun.symbol)
      PyNew(className, ctorName, pyArgs)(pos)

  private def genArrayFactoryApply(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if sym.exists && sym.owner == defn.ArrayModuleClass && sym.name == nme.apply then
      app.args match
        case List(seq: JavaSeqLiteral) =>
          Some(genArrayLiteral(app.tpe, seq.elems, pos))
        case _ =>
          None
    else
      None

  private def genRuntimeNewArray(app: Apply, pos: PyPosition): PyTree =
    val Apply(_, args) = app
    args match
      case List(_, Literal(arrayClassConstant), dimsArray: JavaSeqLiteral) =>
        dimsArray.elems match
          case singleDim :: Nil =>
            PyNewArray(arrayElemTypeRef(arrayClassConstant.typeValue), genExpr(singleDim))(pos)
          case _ =>
            PyApplyExternal(
              PyExternalName("_scpy_new_multi_array"),
              List(
                PyClassOf(arrayBaseTypeRef(arrayClassConstant.typeValue))(pos),
                PyArrayValue(PyPrimRef.IntRef, genArgsPreservingOrder(dimsArray.elems))(pos)
              )
            )(PyArrayType, pos)
      case _ =>
        genNormalApply(app, pos)

  private def genReflectArrayNewInstance(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if Intrinsics.isArrayNewInstance(sym) then
      app.args match
        case List(componentType, length) if length.tpe.typeSymbol == defn.IntClass =>
          Some(
            PyApplyExternal(
              PyExternalName("_scpy_new_array"),
              List(genExpr(componentType), genExpr(length))
            )(PyArrayType, pos)
          )
        case List(componentType, dimensions) if isIntArrayType(dimensions.tpe) =>
          Some(
            PyApplyExternal(
              PyExternalName("_scpy_new_multi_array"),
              List(genExpr(componentType), genExpr(dimensions))
            )(PyArrayType, pos)
          )
        case _ =>
          None
    else
      None

  private def genArrayLiteral(arrayTp: Type, elems: List[Tree], pos: PyPosition): PyTree =
    PyArrayValue(arrayElemTypeRef(arrayTp), genArgsPreservingOrder(elems))(pos)

  private def arrayElemTypeRef(arrayTp: Type): PyTypeRef =
    arrayTp match
      case JavaArrayType(elemTp) =>
        encoding.encodeTypeRef(elemTp)
      case _ =>
        PyClassRef(PyClassName.ObjectClass)

  private def arrayBaseTypeRef(arrayTp: Type): PyTypeRef =
    arrayTp match
      case JavaArrayType(elemTp) =>
        elemTp match
          case _: JavaArrayType => arrayBaseTypeRef(elemTp)
          case _                => encoding.encodeTypeRef(elemTp)
      case _ =>
        PyClassRef(PyClassName.ObjectClass)

  private def isIntArrayType(tp: Type): Boolean =
    tp.widenDealias match
      case JavaArrayType(elemTp) => elemTp.typeSymbol == defn.IntClass
      case _                     => false

  private def genFacadeNew(app: Apply, pos: PyPosition): PyTree =
    val Apply(Select(New(tpt), _), args) = app: @unchecked
    val classSym = tpt.tpe.typeSymbol
    val binding = encoding.externBindingOf(classSym).get
    PyApplyDynamic(
      genExternRef(binding, tpt.tpe, pos),
      genArgsPreservingOrder(args),
      Nil
    )(encoding.encodeType(app.tpe), pos)

  private def genNormalApply(app: Apply, pos: PyPosition): PyTree =
    val sym = app.fun.symbol
    genDynamicApply(app, pos).getOrElse {
      boundary:
        val args = genArgsPreservingOrder(app.args)
        val methodName = encoding.encodeMethodName(sym)
        val ownerName  = encoding.encodeClassName(sym.owner)
        val resultTpe  = encoding.encodeType(sym.info.finalResultType)
        val isStaticTarget = isStaticMember(sym) && !isAnonfunDemotedFromStatic(sym)

        genToStringSpecial(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genGetClassSpecial(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genHashCodeSpecial(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genTupleCallOpt(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genPyMapCallOpt(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genPyListCallOpt(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        genPyTupleCallOpt(app, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        if !isStaticTarget && sym.name == nme.clone_ then
          app.fun match
            case Select(receiver, _) =>
              receiver.tpe.widenDealias match
                case JavaArrayType(_) =>
                  break(PyApplyExternal(PyExternalName("_scpy_array_clone"), List(genExpr(receiver)))(PyArrayType, pos))
                case _ =>
                  ()
            case _ =>
              ()

        Intrinsics.applyStaticIntrinsic(sym, args, pos) match
          case Some(tree) => break(tree)
          case None       => ()

        // Intercept `java.lang.String` instance methods — the runtime
        // receiver is a Python `str` which has no `length__I`/`substring__I_I__…`
        // etc. Map the most common calls to Python-native equivalents.
        if Intrinsics.isStringInstanceMethod(sym, isStaticTarget) then
          app.fun match
            case Select(qual, _) =>
              break(genStringCall(sym, genExpr(qual), args, resultTpe, pos))
            case _ => ()
        else if Intrinsics.isCharSequenceInstanceMethod(sym, isStaticTarget) then
          // `cs: CharSequence` may carry a Python `str` at runtime
          // (e.g. when stdlib code like Regex stores a String in a
          // `CharSequence` field). The encoded method name doesn't resolve
          // on `str`, so route through polymorphic `_scpy_charseq_*`
          // helpers that fall back to the encoded method for non-`str`
          // receivers.
          app.fun match
            case Select(qual, _) =>
              genCharSequenceCall(sym, genExpr(qual), args, resultTpe, pos) match
                case Some(tree) => break(tree)
                case None       => ()
            case _ => ()
        else if Intrinsics.isBoxedPrimitiveInstanceMethod(sym, isStaticTarget) then
          // `(d: java.lang.Double).isNaN()` etc. — the runtime receiver is
          // a raw Python `float`/`int`/`bool` because boxing is identity
          // on this backend. Route through polymorphic `_scpy_Double_*` /
          // `_scpy_Boolean_*` helpers that dispatch on `isinstance` and
          // fall back to the encoded method for real ported boxes.
          app.fun match
            case Select(qual, _) =>
              genBoxedPrimitiveCall(sym, genExpr(qual), args, resultTpe, pos) match
                case Some(tree) => break(tree)
                case None       => ()
            case _ => ()
        else if Intrinsics.isStringStaticMethod(sym, isStaticTarget) then
          break(genStringStaticCall(sym, args, resultTpe, pos))

        app.fun match
          case _ if isStaticTarget =>
            PyApplyStatic(
              PyApplyFlags.empty,
              ownerName,
              methodName,
              args
            )(resultTpe, pos)

          case Select(receiver, _) =>
            // Re-anchor `className` to the receiver's static type when
            // `sym.owner` resolves to a runtime-provided abstract parent
            // (e.g. `java.lang.AbstractStringBuilder`). dotc's classfile
            // reader skips `ACC_BRIDGE | ACC_SYNTHETIC` redeclarations on
            // JDK 25 `StringBuilder`/`StringBuffer`, so methods like
            // `setCharAt`/`length`/`charAt`/`capacity`/`getChars` resolve
            // up to `AbstractStringBuilder`. That class is opaque to
            // reachability (`PyIRRuntime.providedClasses`), so the linker
            // would prune the pylib-supplied implementations on the
            // concrete subclass. Anchoring the call to the concrete static
            // receiver lets reachability find pylib's declaration.
            val dispatchOwner =
              dispatchOwnerNameOpt(ownerName, receiver.tpe).getOrElse(ownerName)
            // LambdaLift+MoveStatics receiver re-anchor: a `JavaStatic`
            // helper hoisted out of a top-level def stays on its module
            // class (`Foo$package$`) but dotc's tree shape after lifting
            // is `Select(This(<empty>), liftedAnonfun)` — the `This` points
            // at the lambda's enclosing PACKAGE, not at the static method's
            // own module. Encoding `This(<empty>)` blindly produces
            // `loadModule(_lessempty_greater)`, which the linker rejects.
            // Routing through `moduleReceiver(sym.owner, …)` recovers the
            // intended module-class instance for empty-package fixtures
            // such as `tests/run/Parser.scala` (no `package` declaration,
            // top-level `given strToToken = token(_)` lazy-val initializer).
            val recvTree: PyTree =
              if sym.is(JavaStatic) && receiver.isInstanceOf[This]
                  && receiver.symbol.is(Package)
                  && sym.owner.is(ModuleClass)
              then moduleReceiver(sym.owner, pos)
              else genExpr(receiver)
            PyApply(
              PyApplyFlags.empty,
              PyDispatch.Virtual,
              recvTree,
              dispatchOwner,
              methodName,
              args
            )(resultTpe, pos)

          case id: Ident =>
            if sym.owner.is(ModuleClass) then
              PyApply(
                PyApplyFlags.empty,
                PyDispatch.Virtual,
                moduleReceiver(sym.owner, pos),
                ownerName,
                methodName,
                args
              )(resultTpe, pos)
            else if sym.owner.isClass then
              // Bare-name call on an instance member — the implicit receiver
              // is `this`. Scala 3's tree form elides `this.` on own-class
              // members inside instance methods (and for val-accessors
              // generated from `val x = ...`).
              //
              // BUT: implicit conversions like `Predef.intWrapper(x)` may
              // also lower to a bare `Ident(intWrapper)` whose `sym.owner`
              // is `LowPriorityImplicits` (a parent class of `Predef`,
              // private[scala] and never extended by user code). In that
              // case the qualifier is the Predef module, not `this`.
              // Recover the actual qualifier from the Ident's TermRef
              // prefix via `desugarIdent` and route through the recovered
              // receiver if it is not a `this.` reference.
              val desugared = tpd.desugarIdent(id)
              desugared match
                case sel @ Select(qual, _) if !qual.isInstanceOf[This] =>
                  PyApply(
                    PyApplyFlags.empty,
                    PyDispatch.Virtual,
                    genExpr(qual),
                    ownerName,
                    methodName,
                    args
                  )(resultTpe, pos)
                case Select(qual: This, _) if qual.symbol != currentClassSym =>
                  // Inlined call: the bare `Ident(bar)` lives in
                  // `currentClassSym` (the inline call site, e.g. `Test_`)
                  // but its TermRef prefix points at a *different* enclosing
                  // class (e.g. `This(Foo)`). Routing through the local
                  // `self` would dispatch on the call site's receiver, which
                  // does not extend the inlined method's owner — at runtime
                  // Python raises `AttributeError`. Reuse `genExpr(qual)`
                  // so cross-class This is lowered correctly: a module class
                  // becomes `PyLoadModule(<ModuleClass>)`, recovering the
                  // intended singleton receiver.
                  PyApply(
                    PyApplyFlags.empty,
                    PyDispatch.Virtual,
                    genExpr(qual),
                    ownerName,
                    methodName,
                    args
                  )(resultTpe, pos)
                case _ =>
                  val classTpe = PyClassType(encoding.encodeClassName(currentClassSym))
                  PyApply(
                    PyApplyFlags.empty,
                    PyDispatch.Virtual,
                    PyThis()(classTpe, pos),
                    ownerName,
                    methodName,
                    args
                  )(resultTpe, pos)
            else
              PyApplyExternal(
                PyExternalName(methodName.encoded),
                args
              )(resultTpe, pos)

          case _ =>
            PyApplyExternal(
              PyExternalName(methodName.encoded),
              args
            )(resultTpe, pos)
    }

  /** When `ownerName` (from `sym.owner`) names a runtime-provided abstract
   *  parent that pylib's concrete subclass does NOT extend, return the
   *  receiver's static class name so virtual dispatch is anchored to the
   *  class the linker actually loads. Returns `None` when no re-anchoring
   *  applies, in which case the caller keeps `sym.owner`.
   *
   *  This is the bridge/synthetic workaround for JDK 25
   *  `StringBuilder`/`StringBuffer`: their `setCharAt`/`length`/`charAt`/
   *  `capacity`/`getChars`/etc. redeclarations are
   *  `ACC_BRIDGE | ACC_SYNTHETIC` forwarders. dotc's classfile reader skips
   *  those, so `sym.owner` resolves up to `AbstractStringBuilder`, which is
   *  an opaque [[PyIRRuntime.providedClasses]] shim. Without re-anchoring,
   *  reachability bails out at the runtime-provided check and the linker
   *  prunes the pylib-supplied implementation, surfacing as
   *  `AttributeError: ... has no attribute 'setCharAt__I_C__V'` at run
   *  time. This is narrowly scoped to the known shim ancestors so generic
   *  inherited-from-`Object` calls (`toString`, `getClass`, `wait`, …) are
   *  unaffected — those classes really do extend `Object` in pylib and
   *  resolve via the runtime's MRO.
   */
  private def dispatchOwnerNameOpt(ownerName: PyClassName, recvTpe: Type): Option[PyClassName] =
    if !GenPython.detachedShimAncestors.contains(ownerName) then None
    else
      val recvSym = recvTpe.widenDealias.typeSymbol
      if !recvSym.exists || !recvSym.isClass then None
      else
        val recvName = encoding.encodeClassName(recvSym)
        if recvName == ownerName then None
        else if PyIRRuntime.providedClass(recvName).isDefined then None
        else Some(recvName)

  /** Map `java.lang.String` instance method calls onto Python-native
   *  equivalents — the runtime receiver is a Python `str`, not a ported
   *  Scala class, so encoded Scala method names like `length__I` don't
   *  resolve. Unmapped methods fall back to attribute access which will
   *  surface as a clean `AttributeError` rather than a silent miscompile.
   */
  private def genStringCall(
      sym: Symbol,
      recv: PyTree,
      args: List[PyTree],
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    val name = sym.name.mangledString
    def regexHelper(helperName: String, helperArgs: List[PyTree]): PyTree =
      genStringRegexHelperCall(helperName, recv :: helperArgs, resultTpe, pos)
    def attr(attrName: String, callArgs: List[PyTree] = args): PyTree =
      PyApplyDynamic(
        PyAttrAccess(recv, attrName)(PyAnyType, pos),
        callArgs, Nil
      )(resultTpe, pos)
    def external(helperName: String, callArgs: List[PyTree] = args): PyTree =
      PyApplyExternal(PyExternalName(helperName), recv :: callArgs)(resultTpe, pos)
    def unsupported(message: String): PyTree =
      PyApplyExternal(
        PyExternalName("_scpy_unsupported"),
        List(PyStringLit(message)(pos))
      )(resultTpe, pos)
    name match
      case "length" =>
        PyApplyExternal(PyExternalName("_scpy_len"), List(recv))(resultTpe, pos)
      case "charAt" =>
        external("_scpy_str_char_at")
      case "codePointAt"       => external("_scpy_str_code_point_at")
      case "codePointBefore"   => external("_scpy_str_code_point_before")
      case "codePointCount"    => external("_scpy_str_code_point_count")
      case "offsetByCodePoints" => external("_scpy_str_offset_by_code_points")
      case "hashCode"          => external("_scpy_str_hash_code", Nil)
      case "equals"            => external("_scpy_str_equals")
      case "equalsIgnoreCase"  => external("_scpy_str_equals_ci")
      case "compareTo"         => external("_scpy_str_compare_to")
      case "compareToIgnoreCase" => external("_scpy_str_compare_to_ci")
      case "concat"            => external("_scpy_str_concat")
      case "contains"          => external("_scpy_str_contains")
      case "startsWith"        => external("_scpy_str_startswith")
      case "endsWith"          => attr("endswith")
      case "indexOf"           => external("_scpy_str_index_of")
      case "lastIndexOf"       => external("_scpy_str_last_index_of")
      case "isEmpty"           => external("_scpy_str_isempty", Nil)
      case "intern"            => recv
      case "substring"         => external("_scpy_str_substring")
      case "subSequence"       => external("_scpy_str_substring")
      case "getChars"          => external("_scpy_str_get_chars")
      case "toCharArray"       => external("_scpy_str_to_char_array", Nil)
      case "toLowerCase" =>
        if args.isEmpty then attr("lower", Nil)
        else unsupported("java.lang.String.toLowerCase(Locale) pending Locale port")
      case "toUpperCase" =>
        if args.isEmpty then attr("upper", Nil)
        else unsupported("java.lang.String.toUpperCase(Locale) pending Locale port")
      case "trim"              => external("_scpy_str_trim", Nil)
      case "strip"             => external("_scpy_str_strip", Nil)
      case "stripLeading"      => external("_scpy_str_strip_leading", Nil)
      case "stripTrailing"     => external("_scpy_str_strip_trailing", Nil)
      case "isBlank"           => external("_scpy_str_is_blank", Nil)
      case "replace"           => external("_scpy_str_replace")
      case "replaceAll" =>
        regexHelper("replaceAllRegex", args)
      case "replaceFirst" =>
        regexHelper("replaceFirstRegex", args)
      case "matches" =>
        regexHelper("matchesRegex", args)
      case "split" =>
        regexHelper("splitRegex", args)
      case "regionMatches"     => external("_scpy_str_region_matches")
      case "repeat"            => external("_scpy_str_repeat")
      case "getBytes"          => external("_scpy_str_get_bytes")
      case "indent"            => external("_scpy_str_indent")
      case "stripIndent"       => external("_scpy_str_strip_indent", Nil)
      case "translateEscapes"  => external("_scpy_str_translate_escapes", Nil)
      case "toString"          => recv
      case _ =>
        // Unmapped: emit attribute access + dynamic call so Python
        // surfaces an AttributeError naming the method precisely.
        attr(name)

  /** Map calls on the `java.lang.CharSequence` interface to polymorphic
   *  runtime helpers. The static receiver type is `CharSequence`, but the
   *  runtime type may be a Python `str` (no encoded methods) or a real
   *  `CharSequence` subclass like `StringBuilder` / `ArrayCharSequence`
   *  (does have encoded methods). Returns `None` for symbols we don't
   *  recognize so the caller falls through to the regular dispatch.
   */
  private def genCharSequenceCall(
      sym: Symbol,
      recv: PyTree,
      args: List[PyTree],
      resultTpe: PyType,
      pos: PyPosition
  ): Option[PyTree] =
    val name = sym.name.mangledString
    def external(helperName: String, callArgs: List[PyTree] = args): PyTree =
      PyApplyExternal(PyExternalName(helperName), recv :: callArgs)(resultTpe, pos)
    name match
      case "length"      => Some(external("_scpy_charseq_length", Nil))
      case "charAt"      => Some(external("_scpy_charseq_char_at"))
      case "subSequence" => Some(external("_scpy_charseq_sub_sequence"))
      case "isEmpty"     => Some(external("_scpy_charseq_is_empty", Nil))
      case "toString"    => Some(external("_scpy_charseq_to_string", Nil))
      case _             => None

  /** Map calls on boxed-primitive instance methods (`java.lang.Double#isNaN`,
   *  `java.lang.Long#intValue`, ...) to polymorphic runtime helpers.
   *  See [[Intrinsics.isBoxedPrimitiveInstanceMethod]] for the predicate
   *  this pairs with. Returns `None` for symbols we don't recognize so
   *  the caller falls through to regular dispatch (which on a real
   *  ported box will succeed by virtue of the `virtualCallSeeds` keeping
   *  the encoded methods alive).
   */
  private def genBoxedPrimitiveCall(
      sym: Symbol,
      recv: PyTree,
      args: List[PyTree],
      resultTpe: PyType,
      pos: PyPosition
  ): Option[PyTree] =
    val name = sym.name.mangledString
    def external(helperName: String): PyTree =
      PyApplyExternal(PyExternalName(helperName), recv :: args)(resultTpe, pos)
    val owner = sym.owner
    name match
      // Numeric narrowings — Number contract; we only need one helper
      // family because all numeric boxes (Number, Double, Float, Long,
      // Integer, Byte, Short) have the same JVM contract on these
      // names: truncate-and-narrow against the runtime numeric value.
      case "intValue"    if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_intValue"))
      case "longValue"   if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_longValue"))
      case "floatValue"  if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_floatValue"))
      case "doubleValue" if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_doubleValue"))
      case "byteValue"   if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_byteValue"))
      case "shortValue"  if owner != defn.BoxedBooleanClass => Some(external("_scpy_Double_shortValue"))
      // Float-specific predicates — only meaningful on Double/Float boxes.
      case "isNaN"      if owner == defn.BoxedDoubleClass || owner == defn.BoxedFloatClass =>
        Some(external("_scpy_Double_isNaN"))
      case "isInfinite" if owner == defn.BoxedDoubleClass || owner == defn.BoxedFloatClass =>
        Some(external("_scpy_Double_isInfinite"))
      // Boolean unboxing.
      case "booleanValue" if owner == defn.BoxedBooleanClass =>
        Some(external("_scpy_Boolean_booleanValue"))
      case _ => None

  private def genStringCtorCall(ctor: Symbol, args: List[PyTree], pos: PyPosition): PyTree =
    val methodName = PyMethodName(
      PySimpleMethodName("new"),
      ctor.info.paramInfoss.flatten.map(encoding.encodeTypeRef),
      PyClassRef(PyClassName.StringClass)
    )
    PyApply(
      PyApplyFlags.empty,
      PyDispatch.Virtual,
      PyLoadModule(stringCompanionClassName)(pos),
      stringCompanionClassName,
      methodName,
      args
    )(PyStringType, pos)

  /** Lower a Scala-tuple-shaped call to native Python tuple ops.
   *
   *  Three call shapes are handled:
   *    1. `Tuple{N}.apply(...)` / `Tuple$.apply(...)` static calls on
   *       a tuple companion module. Lower to `PyTupleValue`.
   *    2. `runtime.Tuples.*` static dispatch helpers (the inline ops
   *       in `library/src/scala/Tuple.scala` forward here). Lower to
   *       `_scpy_tuple_*` runtime helpers in the prelude.
   *    3. Instance methods on a Tuple-typed receiver: `_1` … `_22`,
   *       `swap`, and the `Product` accessors (`productArity`,
   *       `productElement`, `productIterator`, `productPrefix`,
   *       `productElementName`). Lower to the same runtime helpers.
   *
   *  Returns `Some(tree)` when the call should be rewritten; `None`
   *  otherwise. Receiver-type checks are static — calls on
   *  `Product`/`Any`-typed receivers fall through to the regular
   *  dispatch (out of scope per the plan).
   */
  private def genTupleCallOpt(app: Apply, pos: PyPosition): Option[PyTree] = boundary:
    val sym = app.fun.symbol
    if !sym.exists then break(None)

    // (1) `Tuple{N}.apply(...)` on the case-class companion module.
    if isTupleCompanionApply(sym) then
      val args = genArgsPreservingOrder(app.args)
      val tpe  = encoding.encodeType(app.tpe)
      break(Some(PyTupleValue(args)(tpe, pos)))

    // (2) `runtime.Tuples.*` static helpers.
    if pyDefn.RuntimeTuplesModule.exists
        && sym.owner == pyDefn.RuntimeTuplesModule.moduleClass
    then
      genTuplesStaticCall(sym, app.args, app.tpe, pos) match
        case s @ Some(_) => break(s)
        case None        => ()

    // (3) Polymorphic `Product` methods. Erasure can collapse a tuple
    //     to a `Product`/`Any` static type at the call site (notably
    //     after `:*` / `++` / match-type evaluation), so receiver-type
    //     dispatch alone is insufficient. Always route Product method
    //     calls through a runtime helper that branches on
    //     `isinstance(x, tuple)`.
    app.fun match
      case Select(receiver, _) =>
        genProductPolyfill(sym, receiver, app.args, app.tpe, pos) match
          case s @ Some(_) => break(s)
          case None        => ()
      case _ =>
        ()

    // (4) Instance methods on a Tuple receiver.
    app.fun match
      case Select(receiver, _) if isTupleReceiverType(receiver.tpe) =>
        genTupleInstanceCall(sym, receiver, app.args, app.tpe, pos)
      case _ =>
        None

  private def genProductPolyfill(
      sym: Symbol,
      receiver: Tree,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] =
    val resultTpe = encoding.encodeType(resultScalaTpe)
    def ext(name: String, ts: List[PyTree]): PyTree =
      PyApplyExternal(PyExternalName(name), ts)(resultTpe, pos)
    // `genExpr(receiver)` may push the receiver's side-effecting
    // sub-trees to `pendingLocalDefs`. Only call it when a polyfill
    // actually fires; otherwise the receiver will be re-evaluated by
    // the regular `genNormalApply` dispatch path, duplicating any side
    // effects (see `tests/run/runtime.scala`'s
    // `{Console.print(23); test1.bar.System}.out.println()`).
    def recv: PyTree = genExpr(receiver)
    if      sym == pyDefn.Product_productArity && args.isEmpty then
      Some(ext("_scpy_product_arity", List(recv)))
    else if sym == pyDefn.Product_productElement && args.length == 1 then
      Some(ext("_scpy_product_element", List(recv, genExpr(args.head))))
    else if sym == pyDefn.Product_productIterator && args.isEmpty then
      Some(ext("_scpy_product_iterator", List(recv)))
    else if sym == pyDefn.Product_productPrefix && args.isEmpty then
      Some(ext("_scpy_product_prefix", List(recv)))
    else if sym == pyDefn.Product_productElementName && args.length == 1 then
      Some(ext("_scpy_product_element_name", List(recv, genExpr(args.head))))
    else
      None

  /** True iff `tpe` resolves to a Scala tuple type. Match-type aliases
   *  like `Append[Tuple22, 23]` and singleton-tuple selections need
   *  `widenDealias` to surface their underlying `*:` chain.
   *  `tupleElementTypes` is dotc's authoritative tuple-shape check. */
  private def isTupleReceiverType(tpe: Type): Boolean = boundary:
    if tpe.tupleElementTypes.isDefined then break(true)
    pyDefn.isTupleClass(tpe.widenDealias.typeSymbol)

  /** True for a fixed-arity tuple class (`Tuple1..22` and specialized
   *  forms like `Tuple2$mcII$sp`).
   *
   *  Deliberately excludes `Tuple` (trait), `NonEmptyTuple` (trait),
   *  `*:` (cons class — distinct shape), `EmptyTuple$`, and
   *  `TupleXXL` (variadic-arity, kept as a real Scala class). */
  private def isFixedArityTupleClass(sym: Symbol): Boolean = boundary:
    if !sym.exists || !sym.isClass then break(false)
    if defn.isTupleClass(sym) then break(true)
    val cs = sym.asClass
    cs != pyDefn.PairClass && cs.derivesFrom(pyDefn.PairClass) && cs != pyDefn.TupleXXLClass

  /** True when `sym` is the `apply(t1, …, tN)` factory on a
   *  fixed-arity `TupleN` companion module (`Tuple1$`..`Tuple22$` and
   *  any specialized form derived from `NonEmptyTuple`).
   *
   *  Deliberately excludes `Tuple$` and `TupleXXL$`: those expose
   *  variadic / collection-shaped factories (`Tuple.apply(Seq)`,
   *  `TupleXXL.apply(Seq)`) whose single Seq argument is NOT the
   *  tuple's element list and would be miscompiled as a 1-tuple. */
  private def isTupleCompanionApply(sym: Symbol): Boolean = boundary:
    if sym.name != nme.apply then break(false)
    val owner = sym.owner
    if !owner.is(ModuleClass) then break(false)
    val companionClass = owner.linkedClass
    isFixedArityTupleClass(companionClass)

  private def genTuplesStaticCall(
      sym: Symbol,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] =
    val resultTpe = encoding.encodeType(resultScalaTpe)
    def ext(name: String): PyTree =
      PyApplyExternal(
        PyExternalName(name),
        genArgsPreservingOrder(args)
      )(resultTpe, pos)
    if      sym == pyDefn.Tuples_apply       then Some(ext("_scpy_tuple_get"))
    else if sym == pyDefn.Tuples_cons        then Some(ext("_scpy_tuple_cons"))
    else if sym == pyDefn.Tuples_append      then Some(ext("_scpy_tuple_append"))
    else if sym == pyDefn.Tuples_concat      then Some(ext("_scpy_tuple_concat"))
    else if sym == pyDefn.Tuples_tail        then Some(ext("_scpy_tuple_tail"))
    else if sym == pyDefn.Tuples_init        then Some(ext("_scpy_tuple_init"))
    else if sym == pyDefn.Tuples_last        then Some(ext("_scpy_tuple_last"))
    else if sym == pyDefn.Tuples_size        then Some(ext("_scpy_tuple_size"))
    else if sym == pyDefn.Tuples_reverse     then Some(ext("_scpy_tuple_reverse"))
    else if sym == pyDefn.Tuples_take        then Some(ext("_scpy_tuple_take"))
    else if sym == pyDefn.Tuples_drop        then Some(ext("_scpy_tuple_drop"))
    else if sym == pyDefn.Tuples_splitAt     then Some(ext("_scpy_tuple_splitat"))
    else if sym == pyDefn.Tuples_zip         then Some(ext("_scpy_tuple_zip"))
    else if sym == pyDefn.Tuples_map         then Some(ext("_scpy_tuple_map"))
    else if sym == pyDefn.Tuples_toArray     then Some(ext("_scpy_tuple_to_array"))
    else if sym == pyDefn.Tuples_toIArray    then Some(ext("_scpy_tuple_to_iarray"))
    else if sym == pyDefn.Tuples_fromArray   then Some(ext("_scpy_tuple_from_array"))
    else if sym == pyDefn.Tuples_fromIArray  then Some(ext("_scpy_tuple_from_iarray"))
    else if sym == pyDefn.Tuples_fromProduct then Some(ext("_scpy_tuple_from_product"))
    else if sym == pyDefn.Tuples_isInstanceOfTuple         then Some(ext("_scpy_isinstance_tuple"))
    else if sym == pyDefn.Tuples_isInstanceOfEmptyTuple    then Some(ext("_scpy_isinstance_empty_tuple"))
    else if sym == pyDefn.Tuples_isInstanceOfNonEmptyTuple then Some(ext("_scpy_isinstance_nonempty_tuple"))
    else None

  private def genTupleInstanceCall(
      sym: Symbol,
      receiver: Tree,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] = boundary:
    val resultTpe = encoding.encodeType(resultScalaTpe)
    val name = sym.name.toString
    def recv: PyTree = genExpr(receiver)
    def ext(helper: String, callArgs: List[PyTree]): PyTree =
      PyApplyExternal(PyExternalName(helper), callArgs)(resultTpe, pos)

    // `_1`..`_22` accessors on Tuple{N}.
    if name.length >= 2 && name.charAt(0) == '_' && name.tail.forall(_.isDigit) then
      val n = name.tail.toInt
      if n >= 1 && n <= 22 && args.isEmpty then
        break(Some(ext("_scpy_tuple_get", List(recv, PyIntLit(n - 1)(pos)))))

    if sym == pyDefn.Tuple2_swap then
      Some(ext("_scpy_tuple_reverse", List(recv)))
    else if name == "productArity" && args.isEmpty then
      Some(ext("_scpy_tuple_size", List(recv)))
    else if name == "productElement" && args.length == 1 then
      Some(ext("_scpy_tuple_get", List(recv, genExpr(args.head))))
    else if name == "productIterator" && args.isEmpty then
      Some(ext("_scpy_tuple_iter", List(recv)))
    else if name == "productPrefix" && args.isEmpty then
      Some(ext("_scpy_tuple_prefix", List(recv)))
    else if name == "productElementName" && args.length == 1 then
      Some(ext("_scpy_tuple_element_name", List(recv, genExpr(args.head))))
    else if name == "hashCode" && args.isEmpty then
      Some(ext("_scpy_tuple_hash", List(recv)))
    else
      None

  /** Lower a `scala.python.runtime.PyMap` call to native Python `dict` ops.
   *
   *  Two call shapes:
   *    1. `PyMap.empty[K, V]()` factory — lower to `PyDictValue(Nil)`.
   *    2. Instance methods on a `PyMap`-typed receiver — dispatch to
   *       a `_scpy_dict_*` runtime helper.
   *
   *  No erasure-collapse concern (`PyMap` shares no common supertype
   *  with `Tuple`/`Product`/`Any`-typed call sites that we'd want to
   *  miss), so the receiver-type check is purely static.
   */
  private def genPyMapCallOpt(app: Apply, pos: PyPosition): Option[PyTree] = boundary:
    val sym = app.fun.symbol
    if !sym.exists then break(None)

    if pyDefn.PyMapModule.exists && sym == pyDefn.PyMap_empty then
      break(Some(PyDictValue(Nil)(encoding.encodeType(app.tpe), pos)))

    app.fun match
      case Select(receiver, _) if isPyMapReceiverType(receiver.tpe) =>
        genPyMapInstanceCall(sym, receiver, app.args, app.tpe, pos)
      case _ =>
        None

  private def isPyMapReceiverType(tpe: Type): Boolean = boundary:
    if !pyDefn.PyMapClass.exists then break(false)
    tpe.widenDealias.typeSymbol == pyDefn.PyMapClass

  private def genPyMapInstanceCall(
      sym: Symbol,
      receiver: Tree,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] =
    val resultTpe = encoding.encodeType(resultScalaTpe)
    // Lazy: avoid pushing the receiver's side-effecting sub-trees into
    // `pendingLocalDefs` when we end up returning None and the regular
    // dispatch re-evaluates the receiver. Same lesson as `genProductPolyfill`.
    def recv: PyTree = genExpr(receiver)
    def ext(name: String, ts: List[PyTree]): PyTree =
      PyApplyExternal(PyExternalName(name), ts)(resultTpe, pos)
    def arg(i: Int): PyTree = genExpr(args(i))

    if      sym == pyDefn.PyMap_size         then Some(ext("_scpy_dict_size",          List(recv)))
    else if sym == pyDefn.PyMap_isEmpty      then Some(ext("_scpy_dict_is_empty",      List(recv)))
    else if sym == pyDefn.PyMap_nonEmpty     then Some(ext("_scpy_dict_non_empty",     List(recv)))
    else if sym == pyDefn.PyMap_apply        then Some(ext("_scpy_dict_get",           List(recv, arg(0))))
    else if sym == pyDefn.PyMap_get          then Some(ext("_scpy_dict_get_or_null",   List(recv, arg(0))))
    else if sym == pyDefn.PyMap_getOrElse    then Some(ext("_scpy_dict_get_or_else",   List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyMap_update       then Some(ext("_scpy_dict_set",           List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyMap_delete       then Some(ext("_scpy_dict_delete",        List(recv, arg(0))))
    else if sym == pyDefn.PyMap_setDefault   then Some(ext("_scpy_dict_set_default",   List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyMap_pop          then Some(ext("_scpy_dict_pop",           List(recv, arg(0))))
    else if sym == pyDefn.PyMap_popOrElse    then Some(ext("_scpy_dict_pop_or_else",   List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyMap_popItem      then Some(ext("_scpy_dict_pop_item",      List(recv)))
    else if sym == pyDefn.PyMap_clear        then Some(ext("_scpy_dict_clear",         List(recv)))
    else if sym == pyDefn.PyMap_updateAll    then Some(ext("_scpy_dict_update_all",    List(recv, arg(0))))
    else if sym == pyDefn.PyMap_contains     then Some(ext("_scpy_dict_contains",      List(recv, arg(0))))
    else if sym == pyDefn.PyMap_copy         then Some(ext("_scpy_dict_copy",          List(recv)))
    else if sym == pyDefn.PyMap_merged       then Some(ext("_scpy_dict_merged",        List(recv, arg(0))))
    else if sym == pyDefn.PyMap_mergeInPlace then Some(ext("_scpy_dict_merge_in_place",List(recv, arg(0))))
    else if sym == pyDefn.PyMap_keys         then Some(ext("_scpy_dict_keys_iter",     List(recv)))
    else if sym == pyDefn.PyMap_values       then Some(ext("_scpy_dict_values_iter",   List(recv)))
    else if sym == pyDefn.PyMap_items        then Some(ext("_scpy_dict_items_iter",    List(recv)))
    else None

  /** Lower a `scala.python.runtime.PyList` call to native Python `list` ops.
   *
   *  Three call shapes:
   *    1. `PyList.empty[T]()` factory — lower to `PyListValue(Nil)`.
   *    2. `PyList(a, b, c)` varargs factory — args arrive as a single
   *       repeated argument (possibly wrapped in `wrap{T}Array`);
   *       `extractRepeatedArgs` peels the wrapping. Lower to
   *       `PyListValue(elems)`.
   *    3. Instance methods on a `PyList`-typed receiver — dispatch to
   *       a `_scpy_list_*` runtime helper.
   */
  private def genPyListCallOpt(app: Apply, pos: PyPosition): Option[PyTree] = boundary:
    val sym = app.fun.symbol
    if !sym.exists then break(None)

    if pyDefn.PyListModule.exists && sym == pyDefn.PyList_empty then
      break(Some(PyListValue(Nil)(encoding.encodeType(app.tpe), pos)))

    if pyDefn.PyListModule.exists && sym == pyDefn.PyList_applyFactory then
      app.args match
        case List(repeated) =>
          val elems = extractRepeatedArgs(repeated).map(genExpr)
          break(Some(PyListValue(elems)(encoding.encodeType(app.tpe), pos)))
        case _ =>
          break(None)

    app.fun match
      case Select(receiver, _) if isPyListReceiverType(receiver.tpe) =>
        genPyListInstanceCall(sym, receiver, app.args, app.tpe, pos)
      case _ =>
        None

  private def isPyListReceiverType(tpe: Type): Boolean = boundary:
    if !pyDefn.PyListClass.exists then break(false)
    tpe.widenDealias.typeSymbol == pyDefn.PyListClass

  private def genPyListInstanceCall(
      sym: Symbol,
      receiver: Tree,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] =
    val resultTpe = encoding.encodeType(resultScalaTpe)
    def recv: PyTree = genExpr(receiver)
    def ext(name: String, ts: List[PyTree]): PyTree =
      PyApplyExternal(PyExternalName(name), ts)(resultTpe, pos)
    def arg(i: Int): PyTree = genExpr(args(i))

    if      sym == pyDefn.PyList_size      then Some(ext("_scpy_list_size",         List(recv)))
    else if sym == pyDefn.PyList_isEmpty   then Some(ext("_scpy_list_is_empty",     List(recv)))
    else if sym == pyDefn.PyList_nonEmpty  then Some(ext("_scpy_list_non_empty",    List(recv)))
    else if sym == pyDefn.PyList_apply     then Some(ext("_scpy_list_get",          List(recv, arg(0))))
    else if sym == pyDefn.PyList_update    then Some(ext("_scpy_list_set",          List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyList_append    then Some(ext("_scpy_list_append",       List(recv, arg(0))))
    else if sym == pyDefn.PyList_prepend   then Some(ext("_scpy_list_prepend",      List(recv, arg(0))))
    else if sym == pyDefn.PyList_insert    then Some(ext("_scpy_list_insert",       List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyList_removeAt  then Some(ext("_scpy_list_remove_at",    List(recv, arg(0))))
    else if sym == pyDefn.PyList_remove    then Some(ext("_scpy_list_remove_value", List(recv, arg(0))))
    else if sym == pyDefn.PyList_clear     then Some(ext("_scpy_list_clear",        List(recv)))
    else if sym == pyDefn.PyList_extend    then Some(ext("_scpy_list_extend",       List(recv, arg(0))))
    else if sym == pyDefn.PyList_contains  then Some(ext("_scpy_list_contains",     List(recv, arg(0))))
    else if sym == pyDefn.PyList_indexOf   then Some(ext("_scpy_list_index_of",     List(recv, arg(0))))
    else if sym == pyDefn.PyList_count     then Some(ext("_scpy_list_count",        List(recv, arg(0))))
    else if sym == pyDefn.PyList_copy      then Some(ext("_scpy_list_copy",         List(recv)))
    else if sym == pyDefn.PyList_concat    then Some(ext("_scpy_list_concat",       List(recv, arg(0))))
    else if sym == pyDefn.PyList_slice     then Some(ext("_scpy_list_slice",        List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyList_sort      then Some(ext("_scpy_list_sort",         List(recv)))
    else if sym == pyDefn.PyList_reverse   then Some(ext("_scpy_list_reverse",      List(recv)))
    else if sym == pyDefn.PyList_iterator  then Some(ext("_scpy_list_iter",         List(recv)))
    else None

  /** Lower a `scala.python.runtime.PyTuple` call to native bare-Python-tuple
   *  ops. Mirrors `genPyListCallOpt` but routes to `_scpy_pytuple_*`
   *  helpers and emits `PyRawTupleValue` (NOT `PyTupleValue`, which
   *  wraps in `_scpy_ScalaTuple`).
   */
  private def genPyTupleCallOpt(app: Apply, pos: PyPosition): Option[PyTree] = boundary:
    val sym = app.fun.symbol
    if !sym.exists then break(None)

    if pyDefn.PyTupleModule.exists && sym == pyDefn.PyTuple_empty then
      break(Some(PyRawTupleValue(Nil)(encoding.encodeType(app.tpe), pos)))

    if pyDefn.PyTupleModule.exists && sym == pyDefn.PyTuple_applyFactory then
      app.args match
        case List(repeated) =>
          val elems = extractRepeatedArgs(repeated).map(genExpr)
          break(Some(PyRawTupleValue(elems)(encoding.encodeType(app.tpe), pos)))
        case _ =>
          break(None)

    app.fun match
      case Select(receiver, _) if isPyTupleReceiverType(receiver.tpe) =>
        genPyTupleInstanceCall(sym, receiver, app.args, app.tpe, pos)
      case _ =>
        None

  private def isPyTupleReceiverType(tpe: Type): Boolean = boundary:
    if !pyDefn.PyTupleClass.exists then break(false)
    tpe.widenDealias.typeSymbol == pyDefn.PyTupleClass

  private def genPyTupleInstanceCall(
      sym: Symbol,
      receiver: Tree,
      args: List[Tree],
      resultScalaTpe: Type,
      pos: PyPosition
  ): Option[PyTree] =
    val resultTpe = encoding.encodeType(resultScalaTpe)
    def recv: PyTree = genExpr(receiver)
    def ext(name: String, ts: List[PyTree]): PyTree =
      PyApplyExternal(PyExternalName(name), ts)(resultTpe, pos)
    def arg(i: Int): PyTree = genExpr(args(i))

    if      sym == pyDefn.PyTuple_size      then Some(ext("_scpy_pytuple_size",      List(recv)))
    else if sym == pyDefn.PyTuple_isEmpty   then Some(ext("_scpy_pytuple_is_empty",  List(recv)))
    else if sym == pyDefn.PyTuple_nonEmpty  then Some(ext("_scpy_pytuple_non_empty", List(recv)))
    else if sym == pyDefn.PyTuple_apply     then Some(ext("_scpy_pytuple_get",       List(recv, arg(0))))
    else if sym == pyDefn.PyTuple_contains  then Some(ext("_scpy_pytuple_contains",  List(recv, arg(0))))
    else if sym == pyDefn.PyTuple_indexOf   then Some(ext("_scpy_pytuple_index_of",  List(recv, arg(0))))
    else if sym == pyDefn.PyTuple_count     then Some(ext("_scpy_pytuple_count",     List(recv, arg(0))))
    else if sym == pyDefn.PyTuple_concat    then Some(ext("_scpy_pytuple_concat",    List(recv, arg(0))))
    else if sym == pyDefn.PyTuple_slice     then Some(ext("_scpy_pytuple_slice",     List(recv, arg(0), arg(1))))
    else if sym == pyDefn.PyTuple_iterator  then Some(ext("_scpy_pytuple_iter",      List(recv)))
    else None

  private def genStringStaticCall(
      sym: Symbol,
      args: List[PyTree],
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    val methodName = PyMethodName(
      PySimpleMethodName(sym.name.mangledString),
      sym.info.paramInfoss.flatten.map(encoding.encodeTypeRef),
      encoding.encodeTypeRef(sym.info.finalResultType)
    )
    genStringValueOfIntrinsic(methodName, args, pos).getOrElse {
      PyApply(
        PyApplyFlags.empty,
        PyDispatch.Virtual,
        PyLoadModule(stringCompanionClassName)(pos),
        stringCompanionClassName,
        methodName,
        args
      )(resultTpe, pos)
    }

  private def genStringValueOfIntrinsic(
      methodName: PyMethodName,
      args: List[PyTree],
      pos: PyPosition
  ): Option[PyTree] =
    if methodName.simple.name != "valueOf" || args.length != 1 then None
    else
      methodName.paramTypeRefs match
        case List(ref) if ref == PyPrimRef.CharRef =>
          Some(PyApplyExternal(PyExternalName("chr"), args)(PyStringType, pos))
        case List(ref) if ref == PyPrimRef.FloatRef || ref == PyPrimRef.DoubleRef =>
          Some(PyApplyExternal(PyExternalName("_scpy_float_to_str"), args)(PyStringType, pos))
        case List(ref)
            if ref == PyPrimRef.BooleanRef ||
              ref == PyPrimRef.ByteRef ||
              ref == PyPrimRef.ShortRef ||
              ref == PyPrimRef.IntRef ||
              ref == PyPrimRef.LongRef ||
              ref == PyClassRef(PyClassName.ObjectClass) =>
          Some(PyApplyExternal(PyExternalName("_scpy_to_str"), args)(PyStringType, pos))
        case _ =>
          None

  private def genStringRegexHelperCall(
      helperName: String,
      args: List[PyTree],
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    val (paramRefs, returnRef) =
      helperName match
        case "matchesRegex" =>
          (
            List(
              PyClassRef(PyClassName.StringClass),
              PyClassRef(PyClassName.StringClass)
            ),
            PyPrimRef.BooleanRef
          )
        case "splitRegex" =>
          val params =
            if args.length == 2 then
              List(
                PyClassRef(PyClassName.StringClass),
                PyClassRef(PyClassName.StringClass)
              )
            else
              List(
                PyClassRef(PyClassName.StringClass),
                PyClassRef(PyClassName.StringClass),
                PyPrimRef.IntRef
              )
          (params, PyArrayRef(PyClassRef(PyClassName.StringClass), 1))
        case "replaceAllRegex" | "replaceFirstRegex" =>
          (
            List(
              PyClassRef(PyClassName.StringClass),
              PyClassRef(PyClassName.StringClass),
              PyClassRef(PyClassName.StringClass)
            ),
            PyClassRef(PyClassName.StringClass)
          )
        case other =>
          report.error(s"Internal error: unknown string regex helper '$other'")
          (Nil, PyPrimRef.VoidRef)
    val methodName = PyMethodName(
      PySimpleMethodName(helperName),
      paramRefs,
      returnRef
    )
    PyApply(
      PyApplyFlags.empty,
      PyDispatch.Virtual,
      PyLoadModule(stringCompanionClassName)(pos),
      stringCompanionClassName,
      methodName,
      args
    )(resultTpe, pos)

  private def isStringCompanionModule(sym: Symbol): Boolean =
    sym.exists && sym.is(Module) && sym.moduleClass == defn.StringModule

  private def isStringStaticOwner(sym: Symbol): Boolean =
    sym == defn.StringClass || sym == defn.StringModule

  private def isStringStaticField(sym: Symbol): Boolean =
    (sym.owner == defn.StringClass || sym.owner == defn.StringModule) && !sym.is(Method)

  /** Stdlib code is typechecked against the JDK shape of `java.lang.System`,
   *  where `out` / `err` / `in` are static fields. In pylib they are backed
   *  by live getters, so rewrite just those field reads to zero-arg static
   *  getter calls on the synthetic `java.lang.System` forwarder.
   *
   *  Owner check goes through `Intrinsics.isSystemOwner`, which covers both
   *  `defn.SystemClass` (the JVM Java class) and its synthetic linked
   *  module class — Scala 3 creates the latter as the term-side handle
   *  for static members of Java classes, so `System.out` references can
   *  land on either depending on access shape. */
  private def isSystemStreamStaticFieldRef(sym: Symbol): Boolean =
    sym.exists &&
      Intrinsics.isSystemOwner(sym.owner) &&
      !sym.is(Method) &&
      !sym.is(Module) &&
      !sym.is(Package) &&
      (sym.name.mangledString == "out" ||
        sym.name.mangledString == "err" ||
        sym.name.mangledString == "in")

  private def isStaticOwnerRef(tree: Tree): Boolean = tree match
    case Ident(_) =>
      tree.symbol.is(Module) && tree.symbol.moduleClass.isStaticOwner
    case Select(qual, _) =>
      isStaticOwnerRef(qual) && tree.symbol.is(Module) && tree.symbol.moduleClass.isStaticOwner
    case _ =>
      false

  private def genStaticFieldGetter(sym: Symbol, resultTp: Type, pos: PyPosition): PyTree =
    PyApplyStatic(
      PyApplyFlags.empty,
      encoding.encodeClassName(sym.owner),
      PyMethodName(
        PySimpleMethodName(encoding.encodeFieldName(sym).simple.name),
        Nil,
        encoding.encodeTypeRef(resultTp)
      ),
      Nil
    )(encoding.encodeType(resultTp), pos)

  // --- TypeApply (isInstanceOf / asInstanceOf) -----------------------

  private def genTypeApply(app: TypeApply): PyTree =
    val TypeApply(fun, targs) = app
    val pos = posOf(app)
    val sym = fun.symbol

    if sym == defn.Any_isInstanceOf then
      val receiver = qualifierOf(fun)
      PyIsInstanceOf(genExpr(receiver), encoding.encodeTypeRef(targs.head.tpe))(pos)
    else if sym == defn.Any_asInstanceOf then
      val receiver = qualifierOf(fun)
      PyAsInstanceOf(genExpr(receiver), encoding.encodeType(targs.head.tpe))(pos)
    else
      genExpr(fun)

  // --- Primitive operations ------------------------------------------

  private def genPrimitiveOp(app: Apply, pos: PyPosition): PyTree =
    val Apply(fun, args) = app
    val receiver = qualifierOf(fun)
    val code = primitives.getPrimitive(app, receiver.tpe)

    if isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code) then
      genSimpleOp(receiver, args, code, pos)
    else if code == CONCAT then
      genStringConcat(receiver, args, pos)
    else if code == HASH then
      PyApplyExternal(PyExternalName("hash"), List(genExpr(receiver)))(PyIntType, pos)
    else if isArrayOp(code) then
      genArrayOp(app, receiver, args, code, pos)
    else if code == SYNCHRONIZED then
      genNormalApply(app, pos)
    else if isCoercion(code) then
      genCoercion(receiver, code, pos)
    else
      genNormalApply(app, pos)

  private def isSynchronizedPrimitive(app: Apply): Boolean =
    if primitives.isPrimitive(app) then
      val receiver = qualifierOf(app.fun)
      primitives.getPrimitive(app, receiver.tpe) == SYNCHRONIZED
    else
      false

  private def genBoxesRunTimeEquals(lhs: PyTree, rhs: PyTree, pos: PyPosition): PyTree =
    val moduleClass = defn.BoxesRunTimeModule.moduleClass
    val staticOwner = moduleClass.linkedClass
    PyApplyStatic(
      PyApplyFlags.empty,
      encoding.encodeClassName(if staticOwner.exists then staticOwner else moduleClass),
      encoding.encodeMethodName(defn.BoxesRunTimeModule_externalEquals),
      List(lhs, rhs)
    )(PyBooleanType, pos)

  private def genSynchronizedStat(app: Apply): PyTree =
    val pos = posOf(app)
    val Apply(fun, List(body)) = app: @unchecked
    val receiver = qualifierOf(fun)
    val (receiverLocals, receiverExpr) = genExprWithPending(receiver)
    val monitorName = freshSynchronizedMonitorName()
    val monitorVar = PyVarRef(monitorName)(PyAnyType, pos)
    val monitorDef = PyVarDef(
      name         = monitorName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = PyAnyType,
      mutable      = false,
      rhs          = PyApplyExternal(
        PyExternalName("_scpy_monitor_for"),
        List(receiverExpr)
      )(PyAnyType, pos)
    )(pos)
    val acquire = PyApplyDynamic(
      PyAttrAccess(monitorVar, "acquire")(PyAnyType, pos),
      Nil,
      Nil
    )(PyAnyType, pos)
    val release = PyApplyDynamic(
      PyAttrAccess(monitorVar, "release")(PyAnyType, pos),
      Nil,
      Nil
    )(PyAnyType, pos)
    val syncStmt = PyTryFinally(genStat(body), release)(pos)
    val prefix = receiverLocals :+ monitorDef :+ acquire
    PyBlock(prefix, syncStmt)(pos)

  private def genSynchronizedExpr(app: Apply): PyTree = boundary:
    val pos = posOf(app)
    val Apply(fun, List(body)) = app: @unchecked
    val receiver = qualifierOf(fun)
    val (receiverLocals, receiverExpr) = genExprWithPending(receiver)
    val monitorName = freshSynchronizedMonitorName()
    val monitorVar = PyVarRef(monitorName)(PyAnyType, pos)
    val monitorDef = PyVarDef(
      name         = monitorName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = PyAnyType,
      mutable      = false,
      rhs          = PyApplyExternal(
        PyExternalName("_scpy_monitor_for"),
        List(receiverExpr)
      )(PyAnyType, pos)
    )(pos)
    val acquire = PyApplyDynamic(
      PyAttrAccess(monitorVar, "acquire")(PyAnyType, pos),
      Nil,
      Nil
    )(PyAnyType, pos)
    val release = PyApplyDynamic(
      PyAttrAccess(monitorVar, "release")(PyAnyType, pos),
      Nil,
      Nil
    )(PyAnyType, pos)
    val prefix = receiverLocals :+ monitorDef :+ acquire
    val syncBody: PyTree =
      if encoding.encodeType(app.tpe) == PyVoidType then
        genStat(body)
      else
        val resultTpe = encoding.encodeType(app.tpe)
        val resultName = freshSynchronizedResultName()
        val resultVar = PyVarRef(resultName)(resultTpe, pos)
        val resultDef = PyVarDef(
          name         = resultName,
          originalName = PyOriginalName.NoOriginalName,
          vtpe         = resultTpe,
          mutable      = true,
          rhs          = defaultValueFor(resultTpe, pos)
        )(pos)
        pendingLocalDefs += resultDef
        pendingLocalDefs += PyBlock(
          prefix,
          PyTryFinally(genAssignFromExpr(resultVar, body, pos), release)(pos)
        )(pos)
        break(resultVar)

    pendingLocalDefs += PyBlock(prefix, PyTryFinally(syncBody, release)(pos))(pos)
    PyUnitLit()(pos)

  private def genExprWithPending(tree: Tree): (List[PyTree], PyTree) =
    withLocalDefScope(genExpr(tree))

  private def genSimpleOp(
      receiver: Tree, args: List[Tree], code: Int, pos: PyPosition
  ): PyTree = boundary:
    import PyBinaryCode.*
    import PyUnaryCode.*

    val receiverType = receiver.tpe
    val lhs = genExpr(receiver)

    args match
      // Unary operations
      case Nil =>
        code match
          case POS => lhs
          case NEG =>
            if encoding.isIntType(receiverType) then PyUnaryOp(IntNeg, lhs)(pos)
            else if encoding.isLongType(receiverType) then PyUnaryOp(LongNeg, lhs)(pos)
            else if encoding.isFloatType(receiverType) then PyUnaryOp(FloatNeg, lhs)(pos)
            else PyUnaryOp(DoubleNeg, lhs)(pos)
          case NOT =>
            if encoding.isIntType(receiverType) then PyUnaryOp(IntNot, lhs)(pos)
            else PyUnaryOp(LongNot, lhs)(pos)
          case ZNOT => PyUnaryOp(BoolNot, lhs)(pos)
          case _    => lhs

      // Binary operations
      case List(rhs) =>
        // Short-circuit `&&`/`||` must NOT evaluate the RHS unconditionally.
        // If the RHS lowers to side-effecting statements + a value (e.g. a
        // Block with assignments), we cannot emit a flat `(lhs and rhs)` —
        // those statements would run unconditionally before the boolean
        // expression. Scope the RHS's pending locals and, when non-empty,
        // hoist to a value-producing `if` that runs the locals only when
        // the LHS dictates evaluation of the RHS.
        if code == ZAND || code == ZOR then
          val (rhsLocals, rhsExprS) = genExprWithPending(rhs)
          if rhsLocals.isEmpty then
            break(PyBinaryOp(if code == ZAND then BoolAnd else BoolOr, lhs, rhsExprS)(pos))
          else
            val resultTpe = encoding.encodeType(defn.BooleanType)
            // `a && b`  =>  if a then b else False
            // `a || b`  =>  if a then True else b
            val falseLit = PyBooleanLit(false)(pos)
            val trueLit  = PyBooleanLit(true)(pos)
            if code == ZAND then
              break(hoistValueIf(lhs, rhsLocals, rhsExprS, Nil, falseLit, resultTpe, pos))
            else
              break(hoistValueIf(lhs, Nil, trueLit, rhsLocals, rhsExprS, resultTpe, pos))
        val rhsExpr = genExpr(rhs)
        val usesUniversalEquality =
          !encoding.isIntType(receiverType) &&
            !encoding.isLongType(receiverType) &&
            !encoding.isFloatType(receiverType) &&
            !encoding.isDoubleType(receiverType) &&
            !encoding.isBooleanType(receiverType) &&
            !encoding.isStringType(receiverType)
        if code == EQ && usesUniversalEquality then
          break(genBoxesRunTimeEquals(lhs, rhsExpr, pos))
        if code == NE && usesUniversalEquality then
          break(PyUnaryOp(BoolNot, genBoxesRunTimeEquals(lhs, rhsExpr, pos))(pos))
        // For arithmetic and comparison, Scala/JVM promotes Int→Long→Float→Double
        // based on the widest operand. Using only the receiver type produces
        // `int / 2.0 → int // int = int`, so derive a dominant type from both sides.
        val rhsType = rhs.tpe
        val domType: Type =
          if encoding.isDoubleType(receiverType) || encoding.isDoubleType(rhsType) then
            defn.DoubleType
          else if encoding.isFloatType(receiverType) || encoding.isFloatType(rhsType) then
            defn.FloatType
          else if encoding.isLongType(receiverType) || encoding.isLongType(rhsType) then
            defn.LongType
          else
            receiverType
        val arithType: Type =
          code match
            case ADD | SUB | MUL | DIV | MOD | LT | LE | GT | GE => domType
            case _ => receiverType
        val op: PyBinaryCode = code match
          // Short-circuit booleans
          case ZOR  => BoolOr
          case ZAND => BoolAnd
          // Reference equality
          case ID => RefEq
          case NI => RefNe
          // Equality dispatched by type
          case EQ =>
            if encoding.isIntType(receiverType) then IntEq
            else if encoding.isLongType(receiverType) then LongEq
            else if encoding.isFloatType(receiverType) then FloatEq
            else if encoding.isDoubleType(receiverType) then DoubleEq
            else if encoding.isBooleanType(receiverType) then BoolEq
            else if encoding.isStringType(receiverType) then StringEq
            else RefEq
          case NE =>
            if encoding.isIntType(receiverType) then IntNe
            else if encoding.isLongType(receiverType) then LongNe
            else if encoding.isFloatType(receiverType) then FloatNe
            else if encoding.isDoubleType(receiverType) then DoubleNe
            else if encoding.isBooleanType(receiverType) then BoolNe
            else RefNe
          case LT =>
            if encoding.isIntType(domType) then IntLt
            else if encoding.isLongType(domType) then LongLt
            else if encoding.isFloatType(domType) then FloatLt
            else DoubleLt
          case LE =>
            if encoding.isIntType(domType) then IntLe
            else if encoding.isLongType(domType) then LongLe
            else if encoding.isFloatType(domType) then FloatLe
            else DoubleLe
          case GT =>
            if encoding.isIntType(domType) then IntGt
            else if encoding.isLongType(domType) then LongGt
            else if encoding.isFloatType(domType) then FloatGt
            else DoubleGt
          case GE =>
            if encoding.isIntType(domType) then IntGe
            else if encoding.isLongType(domType) then LongGe
            else if encoding.isFloatType(domType) then FloatGe
            else DoubleGe
          // Arithmetic (widened to the dominant operand type)
          case ADD =>
            if encoding.isIntType(domType) then IntAdd
            else if encoding.isLongType(domType) then LongAdd
            else if encoding.isFloatType(domType) then FloatAdd
            else DoubleAdd
          case SUB =>
            if encoding.isIntType(domType) then IntSub
            else if encoding.isLongType(domType) then LongSub
            else if encoding.isFloatType(domType) then FloatSub
            else DoubleSub
          case MUL =>
            if encoding.isIntType(domType) then IntMul
            else if encoding.isLongType(domType) then LongMul
            else if encoding.isFloatType(domType) then FloatMul
            else DoubleMul
          case DIV =>
            if encoding.isIntType(domType) then IntDiv
            else if encoding.isLongType(domType) then LongDiv
            else if encoding.isFloatType(domType) then FloatDiv
            else DoubleDiv
          case MOD =>
            if encoding.isIntType(domType) then IntMod
            else if encoding.isLongType(domType) then LongMod
            else if encoding.isFloatType(domType) then FloatMod
            else DoubleMod
          // Bitwise
          case OR  => if encoding.isIntType(receiverType) then IntOr  else LongOr
          case AND => if encoding.isIntType(receiverType) then IntAnd else LongAnd
          case XOR => if encoding.isIntType(receiverType) then IntXor else LongXor
          // Shifts
          case LSL => if encoding.isIntType(receiverType) then IntShl else LongShl
          case ASR => if encoding.isIntType(receiverType) then IntShr else LongShr
          case LSR => if encoding.isIntType(receiverType) then IntUShr else LongUShr
          case _   => RefEq  // fallback - unreachable in practice
        PyBinaryOp(op, lhs, rhsExpr)(pos)

      case other =>
        // Scala primitive ops are nullary (unary: POS/NEG/NOT/ZNOT) or
        // binary; any other arity is a backend invariant violation.
        report.error(
          s"Python backend: unexpected primitive op (code=$code) with ${other.length} arguments",
          sourcePosOf(pos)
        )
        PyUnitLit()(pos)

  /** `toString` is a Scala method, not Python's `__str__`. Most concrete
   *  receivers should use normal virtual dispatch to
   *  `toString__Ljava_lang_String`; only raw Python-backed values need a
   *  helper because they have no Scala-shaped method table. */
  private def genToStringSpecial(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if !isNullaryToString(sym) || isStaticMember(sym) then None
    else
      app.fun match
        case Select(receiver, _) =>
          val receiverType = receiver.tpe.widenDealias
          val receiverExpr = genExpr(receiver)
          if encoding.isStringType(receiverType) then
            Some(receiverExpr)
          else if isUnitValueType(receiverType) then
            Some(PyStringLit("()")(pos))
          else if encoding.isCharType(receiverType) then
            Some(PyApplyExternal(PyExternalName("chr"), List(receiverExpr))(PyStringType, pos))
          else if isFloatValueType(receiverType) then
            Some(PyApplyExternal(PyExternalName("_scpy_float_to_str"), List(receiverExpr))(PyStringType, pos))
          else if isPrimitiveValueType(receiverType) then
            Some(PyApplyExternal(PyExternalName("_scpy_to_str"), List(receiverExpr))(PyStringType, pos))
          else if isRawPythonToStringType(receiverType) then
            Some(PyApplyExternal(PyExternalName("_scpy_call_to_string"), List(receiverExpr))(PyStringType, pos))
          else
            None
        case _ =>
          None

  private def isNullaryToString(sym: Symbol): Boolean =
    sym.exists && sym.name.mangledString == "toString" && sym.info.paramInfoss.flatten.isEmpty

  private def isUnitValueType(tp: Type): Boolean =
    val sym = tp.widenDealias.typeSymbol
    sym == defn.UnitClass || sym == defn.BoxedUnitClass

  private def isFloatValueType(tp: Type): Boolean =
    val sym = tp.widenDealias.typeSymbol
    sym == defn.FloatClass ||
    sym == defn.BoxedFloatClass ||
    sym == defn.DoubleClass ||
    sym == defn.BoxedDoubleClass

  private def isPrimitiveValueType(tp: Type): Boolean =
    val sym = tp.widenDealias.typeSymbol
    sym == defn.BooleanClass ||
    sym == defn.ByteClass ||
    sym == defn.ShortClass ||
    sym == defn.IntClass ||
    sym == defn.LongClass

  private def isRawPythonToStringType(tp: Type): Boolean =
    val sym = tp.widenDealias.typeSymbol
    sym == defn.ObjectClass ||
    sym == charSequenceClass ||
    sym == defn.AnyClass ||
    sym == defn.AnyValClass ||
    sym == defn.BoxedBooleanClass ||
    sym == defn.BoxedByteClass ||
    sym == defn.BoxedShortClass ||
    sym == defn.BoxedIntClass ||
    sym == defn.BoxedLongClass ||
    sym == defn.BoxedFloatClass ||
    sym == defn.BoxedDoubleClass

  /** `Object.getClass()` is normally lowered to a direct method call
   *  `obj.getClass__Ljava_dlang_dClass()`. That works for user-defined
   *  classes, but fails for raw Python primitives (`int`, `str`, etc.)
   *  which have no Scala method namespace. When the receiver's static
   *  type is one of `Any` / `AnyVal` / `Object` / `Matchable` /
   *  the boxed value classes / `String`, route to `_scpy_get_class`. */
  private def genGetClassSpecial(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if !isNullaryGetClass(sym) || isStaticMember(sym) then None
    else
      val receiver = qualifierOf(app.fun)
      if receiver.isEmpty then None
      else
        val receiverType = receiver.tpe.widenDealias
        if isRawPythonGetClassType(receiverType) then
          Some(PyApplyExternal(
            PyExternalName("_scpy_get_class"),
            List(genExpr(receiver))
          )(PyAnyType, pos))
        else None

  private def isNullaryGetClass(sym: Symbol): Boolean =
    sym.exists && sym.name.mangledString == "getClass" && sym.info.paramInfoss.flatten.isEmpty

  private def isRawPythonGetClassType(tp: Type): Boolean =
    val sym = tp.widenDealias.typeSymbol
    sym == defn.AnyClass || sym == defn.AnyValClass ||
    sym == defn.MatchableClass || sym == defn.ObjectClass ||
    sym == defn.StringClass || sym == charSequenceClass ||
    sym == defn.BoxedBooleanClass || sym == defn.BoxedByteClass ||
    sym == defn.BoxedShortClass || sym == defn.BoxedIntClass ||
    sym == defn.BoxedLongClass || sym == defn.BoxedFloatClass ||
    sym == defn.BoxedDoubleClass

  /** `Object.hashCode()` invoked on a receiver whose static type is
   *  `Any` / `AnyVal` / `Object` / `Matchable` / `String` / one of the
   *  boxed value classes does NOT virtually dispatch to a JVM-faithful
   *  algorithm: Python primitives (`int`, `float`, `str`, `bool`) have
   *  built-in `__hash__` implementations that are SALTED (strings),
   *  collapsed (`hash(-1) == -2`), or representation-based
   *  (`Float.hashCode` should be `floatToIntBits` but Python `float.__hash__`
   *  hashes the rational value). The JVM contract is that `Any#hashCode`
   *  matches `Statics.anyHash` for Numbers and `x.hashCode()` for the
   *  rest (which on the JVM is the type-correct algorithm). Route through
   *  `_scpy_any_hash_code`, which mirrors `Statics.anyHash` and falls
   *  back to a virtual `__hash__()` for non-primitive Scala objects.
   *
   *  Receivers whose static type is more specific (e.g. a user case
   *  class, or a typed `String`) keep their normal dispatch — the
   *  String case is already specialised by `genStringCall` to
   *  `_scpy_str_hash_code`, and case classes have a synthesized
   *  `__hash__` that already matches the JVM. */
  private def genHashCodeSpecial(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if !isNullaryHashCode(sym) || sym.is(JavaStatic) then None
    else
      val receiver = qualifierOf(app.fun)
      if receiver.isEmpty then None
      else
        val receiverType = receiver.tpe.widenDealias
        if isRawPythonGetClassType(receiverType) then
          Some(PyApplyExternal(
            PyExternalName("_scpy_any_hash_code"),
            List(genExpr(receiver))
          )(PyIntType, pos))
        else None

  private def isNullaryHashCode(sym: Symbol): Boolean =
    sym.exists && sym.name.mangledString == "hashCode" && sym.info.paramInfoss.flatten.isEmpty

  /** String concatenation. The receiver is always String (post-erasure);
   *  wrap any non-String operand in `_scpy_to_str` so Python `+` succeeds.
   *  `Char` operands get `chr(…)` instead — our encoding stores Chars as
   *  ints, and Scala's `String + Char` semantics require the 1-character
   *  rendering, not the numeric one. */
  private def genStringConcat(receiver: Tree, args: List[Tree], pos: PyPosition): PyTree =
    def asString(t: Tree): PyTree =
      val e = genExpr(t)
      if isUnitValueType(t.tpe) then
        PyStringLit("()")(pos)
      else if e.tpe == PyStringType then e
      else if encoding.isCharType(t.tpe) then
        PyApplyExternal(PyExternalName("chr"), List(e))(PyStringType, pos)
      else if isFloatValueType(t.tpe) then
        PyApplyExternal(PyExternalName("_scpy_float_to_str"), List(e))(PyStringType, pos)
      else
        PyApplyExternal(PyExternalName("_scpy_to_str"), List(e))(PyStringType, pos)
    PyBinaryOp(PyBinaryCode.StringConcat, asString(receiver), asString(args.head))(pos)

  private def genArrayOp(
      app: Apply, receiver: Tree, args: List[Tree], code: Int, pos: PyPosition
  ): PyTree =
    if isArrayLength(code) then
      PyUnaryOp(PyUnaryCode.ArrayLength, genExpr(receiver))(pos)
    else if isArrayGet(code) then
      val elemTpe = encoding.encodeType(app.tpe)
      PyArraySelect(genExpr(receiver), genExpr(args.head))(elemTpe, pos)
    else if isArraySet(code) then
      // Array stores are Unit-typed but routinely appear in expression
      // position (e.g. as a Match-arm body, or a then/else branch of an
      // `If` rendered as a Python ternary). Mirror the `WhileDo | Assign`
      // hoisting in `genExpr`: push the assignment to `pendingLocalDefs`
      // so the existing If/Match hoisting can materialise statement-form
      // code, and supply a Unit value in its place. Without this, a bare
      // `PyAssign` returned as the value of an `Apply` falls through the
      // emitter's `exprToStr` and gets silently dropped to `None`.
      pendingLocalDefs += PyAssign(
        PyArraySelect(genExpr(receiver), genExpr(args(0)))(PyAnyType, pos),
        genExpr(args(1))
      )(pos)
      PyUnitLit()(pos)
    else if isArrayNew(code) then
      val elemRef: PyTypeRef = code match
        case NEW_ZARRAY => PyPrimRef.BooleanRef
        case NEW_BARRAY => PyPrimRef.ByteRef
        case NEW_SARRAY => PyPrimRef.ShortRef
        case NEW_CARRAY => PyPrimRef.CharRef
        case NEW_IARRAY => PyPrimRef.IntRef
        case NEW_LARRAY => PyPrimRef.LongRef
        case NEW_FARRAY => PyPrimRef.FloatRef
        case NEW_DARRAY => PyPrimRef.DoubleRef
        case _          => PyClassRef(PyClassName.ObjectClass)
      PyNewArray(elemRef, genExpr(args.head))(pos)
    else
      PyUnitLit()(pos)

  private def genCoercion(receiver: Tree, code: Int, pos: PyPosition): PyTree =
    import PyUnaryCode.*
    val src = genExpr(receiver)
    def unboxChar(t: PyTree): PyTree =
      PyApplyExternal(PyExternalName("_scpy_unbox_char"), List(t))(PyIntType, pos)
    code match
      // Identity coercions. `C2C` strips a possible `_scpy_Char`
      // wrapper so a value freshly read from a primitive `Array[Char]`
      // (which auto-boxes for the boxed-toString path) doesn't carry
      // its glyph-rendering hook into a primitive Char position.
      case B2B | S2S | I2I | L2L | F2F | D2D => src
      case C2C => unboxChar(src)

      // Widening to Int - pass through (Python int is big). Char must
      // also unwrap so `c.toInt + ":"` renders as `"97"`, not `"a"`.
      case B2I | S2I => src
      case C2I => unboxChar(src)
      case L2I => PyUnaryOp(LongToInt, src)(pos)
      case F2I => PyUnaryOp(FloatToInt, src)(pos)
      case D2I => PyUnaryOp(DoubleToInt, src)(pos)

      // Widening to Long
      case B2L | S2L | I2L => PyUnaryOp(IntToLong, src)(pos)
      case C2L => PyUnaryOp(IntToLong, unboxChar(src))(pos)
      case F2L => PyUnaryOp(FloatToLong, src)(pos)
      case D2L => PyUnaryOp(DoubleToLong, src)(pos)

      // Widening to Float
      case B2F | S2F | I2F => PyUnaryOp(IntToFloat, src)(pos)
      case C2F => PyUnaryOp(IntToFloat, unboxChar(src))(pos)
      case L2F => PyUnaryOp(LongToFloat, src)(pos)
      case D2F => PyUnaryOp(DoubleToFloat, src)(pos)

      // Widening to Double
      case B2D | S2D | I2D => PyUnaryOp(IntToDouble, src)(pos)
      case C2D => PyUnaryOp(IntToDouble, unboxChar(src))(pos)
      case L2D => PyUnaryOp(LongToDouble, src)(pos)
      case F2D => PyUnaryOp(FloatToDouble, src)(pos)

      // Narrowing to Byte / Short / Char
      case I2B | L2B | F2B | D2B | S2B | C2B => PyUnaryOp(IntToByte, src)(pos)
      case I2S | L2S | F2S | D2S | B2S | C2S => PyUnaryOp(IntToShort, src)(pos)
      case I2C | L2C | F2C | D2C | B2C | S2C => PyUnaryOp(IntToChar, src)(pos)

      case _ => src

  // --- Exception handling --------------------------------------------

  /** Lower a `Try` tree given an arm lowerer.
   *
   *  `lowerArm` transforms each arm's body tree (the `try` block and every
   *  catch body) into the PyTree that arm runs. For statement-position
   *  `try/catch`, pass `genStat`. For expression-position, pass a lowerer
   *  that assigns the arm's value to a synthesised temp (see `genTryExpr`).
   *
   *  `lowerArm` is responsible for scoping any side-effect pendings
   *  produced during generation so they stay within the arm (otherwise an
   *  exception thrown by a body-level pending would escape the try). */
  private def genTryShared(tree: Try, lowerArm: Tree => PyTree): PyTree =
    val Try(block, catches, finalizer) = tree
    val pos = posOf(tree)
    val bodyTree = lowerArm(block)

    val tryCatch: PyTree =
      if catches.isEmpty then bodyTree
      else
        val exVar = PyLocalName("_scpy_ex")
        val exVarRef = PyVarRef(exVar)(PyAnyType, pos)

        val handler = catches.foldRight[PyTree](
          PyUnaryOp(PyUnaryCode.Throw, exVarRef)(pos)  // default: rethrow
        ) { (caseDef, elsePart) =>
          val CaseDef(pat, guard, body) = caseDef
          // Post-erasure invariant: try/catch guards are lifted into the
          // case body before this backend phase, so `guard` should always
          // be `EmptyTree`. Assert loudly so a real miscompile from a stray
          // guard surfaces as a backend error rather than silent dropping.
          assert(
            guard.isEmpty,
            s"Python backend: try/catch CaseDef has a non-empty guard at ${caseDef.sourcePos.show}: ${guard.show}"
          )
          val (exnTypeRef, bindOpt): (Option[PyTypeRef], Option[Symbol]) = pat match
            case Typed(Ident(nme.WILDCARD), tpt) =>
              (Some(encoding.encodeTypeRef(tpt.tpe)), None)
            case Ident(nme.WILDCARD) =>
              (None, None)
            case Bind(_, Typed(_, tpt)) =>
              (Some(encoding.encodeTypeRef(tpt.tpe)), Some(pat.symbol))
            case Bind(_, _) =>
              (None, Some(pat.symbol))
            case _ =>
              (None, None)

          val handlerBody: PyTree = bindOpt match
            case Some(bindSym) =>
              val bindDef = PyVarDef(
                encoding.encodeLocalName(bindSym),
                encoding.originalNameOf(bindSym),
                PyAnyType, false, exVarRef
              )(pos)
              PyBlock(List(bindDef), lowerArm(body))(pos)
            case None =>
              lowerArm(body)

          exnTypeRef match
            case Some(ref) =>
              PyIf(
                PyIsInstanceOf(exVarRef, ref)(pos),
                handlerBody,
                elsePart
              )(PyVoidType, pos)
            case None =>
              handlerBody  // catch-all
        }

        PyTryCatch(
          bodyTree, exVar, PyOriginalName.NoOriginalName, handler
        )(PyVoidType, pos)

    if finalizer.isEmpty then tryCatch
    else PyTryFinally(tryCatch, genStat(finalizer))(pos)

  private def genTry(tree: Try): PyTree =
    genTryShared(tree, genStat)

  /** Lower an expression-position `Try` to a `PyVarRef` that reads from a
   *  synthesised `_scpy_try_result_N` temp. The temp is declared (with the
   *  type's default) and the try-statement (which assigns the temp in every
   *  arm) are pushed to `pendingLocalDefs` so they execute before the
   *  surrounding statement consumes the returned ref.
   *
   *  PyIR `PyTryCatch` / `PyTryFinally` remain statement-shaped; the
   *  expression-ness is encoded by the surrounding temp-assign machinery. */
  private def genTryExpr(tree: Try): PyTree =
    val pos = posOf(tree)
    val resultTpe = encoding.encodeType(tree.tpe)
    val tempName = freshTryResultName()
    val tempLhs = PyVarRef(tempName)(resultTpe, pos)

    val tempDef = PyVarDef(
      name         = tempName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = resultTpe,
      mutable      = true,
      rhs          = defaultValueFor(resultTpe, pos)
    )(pos)

    val tryStmt = genTryShared(tree, body => genAssignFromExpr(tempLhs, body, pos))

    pendingLocalDefs += tempDef
    pendingLocalDefs += tryStmt
    tempLhs

  /** Build `<lhs> = <expr>` as a statement, scoping any pendings produced
   *  during `genExpr(expr)` into a local `PyBlock` so they cannot leak
   *  outside the surrounding try-arm. */
  private def genAssignFromExpr(lhs: PyAssignable, expr: Tree, pos: PyPosition): PyTree =
    val (locals, value) = withLocalDefScope(genExpr(expr))
    val assign = PyAssign(lhs, value)(pos)
    if locals.isEmpty then assign
    else PyBlock(locals, assign)(pos)

  private var tryResultCounter = 0
  private def freshTryResultName(): PyLocalName =
    tryResultCounter += 1
    PyLocalName(s"_scpy_try_result_$tryResultCounter")

  private var synchronizedMonitorCounter = 0
  private def freshSynchronizedMonitorName(): PyLocalName =
    synchronizedMonitorCounter += 1
    PyLocalName(s"_scpy_monitor_$synchronizedMonitorCounter")

  private var synchronizedResultCounter = 0
  private def freshSynchronizedResultName(): PyLocalName =
    synchronizedResultCounter += 1
    PyLocalName(s"_scpy_sync_result_$synchronizedResultCounter")

  private var loopFlagCounter = 0
  private def freshLoopFlagName(): PyLocalName =
    loopFlagCounter += 1
    PyLocalName(s"_scpy_loop_keep_$loopFlagCounter")

  /** Lower an expression-position `Labeled` to a temp-var assign +
   *  statement-position `PyLabeled`.
   *
   *  Scala 3's `PatternMatcher` lowers `expr match { … }` in
   *  expression position to `Labeled(matchEnd, { stats; fallthrough })`
   *  where every exit path is an explicit `Return(matchEnd, result)`
   *  (non-exhaustive matches get a synthesised `throw MatchError` as
   *  the fallthrough — still covered).
   *
   *  Strategy: synthesise `_scpy_labeled_result_N: T = <default>`,
   *  rewrite each `PyLabelReturn(label, v)` in the lowered body to
   *  `{ temp = v; PyLabelReturn(label, ()) }` — i.e. store the value,
   *  then escape — and emit a Void-typed `PyLabeled(label, body)`.
   *  The emitter wraps the body in `try: body except
   *  _scpy_lbl_<n>: pass`, so the temp holds the final value when
   *  control falls out. The expression value is `PyVarRef(temp)`.
   *
   *  Nesting-safe: each `Labeled` gets its own exception class at
   *  emit time (see `PyIREmitter.allocLabelClass`), so nested
   *  `Labeled(a, … Labeled(b, … Return(a, …) …) …)` correctly routes
   *  the outer escape through the inner `except` clause unhandled. */
  private def genLabeledExpr(tree: Labeled): PyTree =
    val Labeled(bind, body) = tree
    val pos = posOf(tree)
    val resultTpe = encoding.encodeType(tree.tpe)
    val labelName = encoding.encodeLabelName(bind.symbol)
    val tempName = freshLabeledResultName()
    val tempVar = PyVarRef(tempName)(resultTpe, pos)
    val tempLhs: PyAssignable = tempVar

    val tempDef = PyVarDef(
      name         = tempName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = resultTpe,
      mutable      = true,
      rhs          = defaultValueFor(resultTpe, pos)
    )(pos)

    // Scope body pendings so they're captured inside the Labeled — a
    // pending side effect inside a match arm must execute only when
    // that arm runs, not unconditionally before the Labeled.
    val (locals, loweredBody) = withLocalDefScope(genStat(body))
    val bodyWithPendings =
      if locals.isEmpty then loweredBody
      else PyBlock(locals, loweredBody)(pos)

    // Two kinds of value-producing exit paths must store into `temp`
    // before the labeled block's emit-time `try/except` clears the
    // exception:
    //
    //   1. Explicit `Return(label, v)` (post-DropBreaks: the rewritten
    //      `break(v)` site, or every PatternMatcher arm). Handled by
    //      `rewriteLabelReturns`.
    //   2. **Fallthrough** — the body's normal-completion value. After
    //      `DropBreaks` the boundary's original `try { ...; tailExpr }`
    //      becomes `Labeled(label, { ...; tailExpr })`, where `tailExpr`
    //      is the value when no `break` fires. PatternMatcher's lowering
    //      doesn't have a fallthrough (every arm explicitly Returns), but
    //      a `boundary` block does, and that path used to drop the
    //      tail-expression value because `genStat` discards it.
    //
    // Apply `wrapFallthroughAssign` after `rewriteLabelReturns` so the
    // already-rewritten label-return blocks (whose tail is now
    // `PyLabelReturn(label, Unit)` — Nothing-typed, doesn't fall through)
    // are correctly identified as non-fallthrough and skipped.
    val rewrittenBody = rewriteLabelReturns(bodyWithPendings, labelName, tempLhs)
    val assignedBody  = wrapFallthroughAssign(rewrittenBody, tempLhs)

    val labeled = PyLabeled(labelName, assignedBody)(PyVoidType, pos)

    pendingLocalDefs += tempDef
    pendingLocalDefs += labeled
    tempVar

  /** Traverse `tree` rewriting every `PyLabelReturn(target, v)` into
   *  `{ PyAssign(tempLhs, v); PyLabelReturn(target, ()) }`. Leaves
   *  other label returns (to unrelated labels) and the rest of the
   *  tree untouched. */
  private def rewriteLabelReturns(
      tree: PyTree, target: PyLabelName, tempLhs: PyAssignable
  ): PyTree = tree match
    case PyLabelReturn(lbl, value) if lbl == target =>
      value match
        case _: PyUnitLit =>
          tree  // already Unit-valued — nothing to assign
        case _ =>
          val p = tree.pos
          PyBlock(
            List(PyAssign(tempLhs, value)(p)),
            PyLabelReturn(lbl, PyUnitLit()(p))(p)
          )(p)

    case PyBlock(stats, expr) =>
      PyBlock(
        stats.map(rewriteLabelReturns(_, target, tempLhs)),
        rewriteLabelReturns(expr, target, tempLhs)
      )(tree.pos)

    case PyIf(cond, thenp, elsep) =>
      PyIf(
        cond,
        rewriteLabelReturns(thenp, target, tempLhs),
        rewriteLabelReturns(elsep, target, tempLhs)
      )(tree.tpe, tree.pos)

    case PyTryCatch(block, errVar, origName, handler) =>
      PyTryCatch(
        rewriteLabelReturns(block, target, tempLhs),
        errVar, origName,
        rewriteLabelReturns(handler, target, tempLhs)
      )(tree.tpe, tree.pos)

    case PyTryFinally(block, finalizer) =>
      PyTryFinally(
        rewriteLabelReturns(block, target, tempLhs),
        rewriteLabelReturns(finalizer, target, tempLhs)
      )(tree.pos)

    case PyMatch(selector, cases, default) =>
      PyMatch(
        selector,
        cases.map { case (lits, body) => (lits, rewriteLabelReturns(body, target, tempLhs)) },
        rewriteLabelReturns(default, target, tempLhs)
      )(tree.tpe, tree.pos)

    case PyLabeled(lbl, body) =>
      // Nested Labeled — recurse so escapes to our target inside its
      // body still get rewritten; its own `PyLabelReturn(lbl, _)`
      // entries (lbl != target) are left alone by the base case above.
      PyLabeled(lbl, rewriteLabelReturns(body, target, tempLhs))(tree.tpe, tree.pos)

    case _ => tree

  /** Wrap every fallthrough tail-position value-producing leaf of `tree`
   *  with `PyAssign(tempLhs, leaf)` so the labeled block's normal
   *  completion stores its value into the temp before the emit-time
   *  `try/except _scpy_lbl_N: pass` discards it.
   *
   *  Walks the structural composers (`PyBlock`, `PyIf`, `PyTryCatch`,
   *  `PyTryFinally`, `PyMatch`) into their tail-position children, even
   *  when those composers carry `tpe = PyVoidType` from `genStat`'s
   *  statement-form lowering: the *node-level* tpe is irrelevant; what
   *  matters is whether a sub-tree's tail position carries a value-
   *  producing leaf. Stops without wrapping at:
   *
   *    - Diverging leaves (`PyReturn`, `PyLabelReturn`, throw via
   *      `PyUnaryOp(Throw, _)`) — they don't fall through.
   *    - Statement-only leaves (`PyAssign`, `PyVarDef`, `PyWhile`,
   *      `PySkip`) — already produced no value to capture; these are
   *      typically the body of a `genStat` Unit-arm.
   *    - Nested `PyLabeled` — its fallthrough is its own concern; its
   *      emit-time `except: pass` swallows its escape and any value it
   *      produced is unobservable from here.
   */
  private def wrapFallthroughAssign(
      tree: PyTree, tempLhs: PyAssignable
  ): PyTree = tree match
    // Diverging leaves — `tpe == PyNothingType` covers `PyReturn`,
    // `PyLabelReturn`, throw, and any other diverging node.
    case _ if tree.tpe == PyNothingType =>
      tree

    // Statement-only leaves: nothing flows to the tail value here.
    case _: (PyAssign | PyVarDef | PyWhile | PySkip) =>
      tree

    case PyBlock(stats, expr) =>
      PyBlock(stats, wrapFallthroughAssign(expr, tempLhs))(tree.pos)

    case PyIf(cond, thenp, elsep) =>
      // Whole `if` becomes statement-shaped after wrapping its arms
      // with assignments, so it now has tpe = PyVoidType.
      PyIf(
        cond,
        wrapFallthroughAssign(thenp, tempLhs),
        wrapFallthroughAssign(elsep, tempLhs)
      )(PyVoidType, tree.pos)

    case PyTryCatch(block, errVar, origName, handler) =>
      PyTryCatch(
        wrapFallthroughAssign(block, tempLhs),
        errVar, origName,
        wrapFallthroughAssign(handler, tempLhs)
      )(PyVoidType, tree.pos)

    case PyTryFinally(block, finalizer) =>
      // The finalizer runs for side effects only — its value never
      // becomes the try/finally's value. Wrap only the protected block.
      PyTryFinally(
        wrapFallthroughAssign(block, tempLhs),
        finalizer
      )(tree.pos)

    case PyMatch(selector, cases, default) =>
      PyMatch(
        selector,
        cases.map { case (lits, body) => (lits, wrapFallthroughAssign(body, tempLhs)) },
        wrapFallthroughAssign(default, tempLhs)
      )(PyVoidType, tree.pos)

    // A nested `PyLabeled` that produces a value is its own assignment
    // story — see the docstring above. Leave it as-is; treat it as a
    // non-fallthrough leaf (its emit-time wrapper swallows its escape).
    case _: PyLabeled =>
      tree

    // PyUnitLit at tail position: the labeled block is value-producing
    // (we wouldn't be here otherwise), so a Unit tail means a discarded
    // value. Skip — assigning `None` would be redundant given the temp
    // was already initialised to its default value.
    case _: PyUnitLit =>
      tree

    // Value-producing leaf in tail position — assign it to the temp.
    case _ =>
      PyAssign(tempLhs, tree)(tree.pos)

  private var labeledResultCounter = 0
  private def freshLabeledResultName(): PyLocalName =
    labeledResultCounter += 1
    PyLocalName(s"_scpy_labeled_result_$labeledResultCounter")

  private var ifResultCounter = 0
  private def freshIfResultName(): PyLocalName =
    ifResultCounter += 1
    PyLocalName(s"_scpy_if_result_$ifResultCounter")

  private var matchResultCounter = 0
  private def freshMatchResultName(): PyLocalName =
    matchResultCounter += 1
    PyLocalName(s"_scpy_match_result_$matchResultCounter")

  private var argTempCounter = 0
  private def freshArgTempName(): PyLocalName =
    argTempCounter += 1
    PyLocalName(s"_scpy_arg_$argTempCounter")

  /** Strict left-to-right evaluation for a sequence of argument trees.
   *
   *  `genExpr` may push statements to `pendingLocalDefs` when it has to
   *  hoist a value-producing `If`/`Match`/`Try`/`Labeled` (multi-stmt
   *  body that can't ride along as a Python expression) to a temp.
   *  Those pushed statements are flushed by the parent before the
   *  enclosing call expression evaluates. So if arg `i` returned an
   *  inline (non-pushed) expression and a later arg `j > i` pushed,
   *  arg `j`'s hoisted statements would run BEFORE arg `i`'s inline
   *  expression evaluates at the call site — reversing Scala's
   *  left-to-right argument order, with side-effect consequences (see
   *  e.g. `tests/run/Course-2002-13.scala`'s `Parser.line`).
   *
   *  Fix: after generating all args, find the smallest `k` whose
   *  generation pushed anything. Every arg `< k` whose result isn't a
   *  pure leaf (`PyLiteral`, `PyVarRef`, `PyThis`) gets lifted to a
   *  fresh `_scpy_arg_N` temp inserted into `pendingLocalDefs`
   *  immediately before arg `k`'s pushed statements, so it evaluates
   *  in source order. Args `>= k` stay inline — they evaluate at the
   *  call site, which is after all hoists, preserving order.
   */
  private def genArgsPreservingOrder(argTrees: List[Tree]): List[PyTree] =
    case class Snap(prefixSize: Int, expr: PyTree)
    val snaps: List[Snap] = argTrees.map { tree =>
      val before = pendingLocalDefs.size
      val expr = genExpr(tree)
      Snap(before, expr)
    }
    val finalSize = pendingLocalDefs.size

    // First arg whose generation grew `pendingLocalDefs`.
    val firstPushIdx = snaps.iterator.zipWithIndex.find { case (snap, i) =>
      val nextPrefix = if i + 1 < snaps.size then snaps(i + 1).prefixSize else finalSize
      nextPrefix > snap.prefixSize
    }.map(_._2)

    firstPushIdx match
      case None => snaps.map(_.expr)
      case Some(k) =>
        val insertAt = snaps(k).prefixSize
        val out = mutable.ArrayBuffer.empty[PyTree]
        var inserted = 0
        for ((snap, i) <- snaps.zipWithIndex)
          if i < k && !isPureArgExpr(snap.expr) then
            val pos = snap.expr.pos
            val tpe = snap.expr.tpe
            val name = freshArgTempName()
            val tempRef = PyVarRef(name)(tpe, pos)
            val tempDef = PyVarDef(
              name         = name,
              originalName = PyOriginalName.NoOriginalName,
              vtpe         = tpe,
              mutable      = false,
              rhs          = snap.expr
            )(pos)
            pendingLocalDefs.insert(insertAt + inserted, tempDef)
            inserted += 1
            out += tempRef
          else
            out += snap.expr
        out.toList
  end genArgsPreservingOrder

  /** Pure leaf — re-evaluating it has no observable side effects, so
   *  ordering relative to a later arg's hoists doesn't matter. */
  private def isPureArgExpr(tree: PyTree): Boolean = tree match
    case _: PyLiteral => true
    case _: PyVarRef  => true
    case _: PyThis    => true
    case _            => false

  /** Hoist a value-producing `If` whose branches carry pending local
   *  definitions into a statement-form `PyIf` that assigns to a fresh
   *  temp, plus a `PyVarRef` reading the temp. This preserves the
   *  invariant that a `PyIf` in expression position never contains a
   *  `PyBlock` (or other statement-shaped node) in its branches —
   *  otherwise the emitter would render it as a Python conditional
   *  expression and silently drop the block's statements. */
  private def hoistValueIf(
      cond: PyTree,
      thenLocals: List[PyTree], thenExpr: PyTree,
      elseLocals: List[PyTree], elseExpr: PyTree,
      resultTpe: PyType, pos: PyPosition
  ): PyTree =
    val tempName = freshIfResultName()
    val tempVar  = PyVarRef(tempName)(resultTpe, pos)
    val tempDef  = PyVarDef(
      name         = tempName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = resultTpe,
      mutable      = true,
      rhs          = defaultValueFor(resultTpe, pos)
    )(pos)
    val thenStmt = stmtsToBody(thenLocals :+ PyAssign(tempVar, thenExpr)(pos), pos)
    val elseStmt = stmtsToBody(elseLocals :+ PyAssign(tempVar, elseExpr)(pos), pos)
    val ifStmt   = PyIf(cond, thenStmt, elseStmt)(PyVoidType, pos)
    pendingLocalDefs += tempDef
    pendingLocalDefs += ifStmt
    tempVar

  /** Hoist a value-producing `Match` whose case bodies carry pending
   *  local definitions into a statement-form `PyMatch` that assigns
   *  each arm's value to a fresh temp, plus a `PyVarRef` reading the
   *  temp. Preserves the same invariant as `hoistValueIf`. */
  private def hoistValueMatch(
      sel: PyTree,
      cases: List[(List[PyMatchableLiteral], List[PyTree], PyTree)],
      defaultLocals: List[PyTree], defaultExpr: PyTree,
      resultTpe: PyType, pos: PyPosition
  ): PyTree =
    val tempName = freshMatchResultName()
    val tempVar  = PyVarRef(tempName)(resultTpe, pos)
    val tempDef  = PyVarDef(
      name         = tempName,
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = resultTpe,
      mutable      = true,
      rhs          = defaultValueFor(resultTpe, pos)
    )(pos)
    val stmtCases = cases.map { case (lits, locals, expr) =>
      (lits, stmtsToBody(locals :+ PyAssign(tempVar, expr)(pos), pos))
    }
    val defaultStmt = stmtsToBody(defaultLocals :+ PyAssign(tempVar, defaultExpr)(pos), pos)
    val matchStmt = PyMatch(sel, stmtCases, defaultStmt)(PyVoidType, pos)
    pendingLocalDefs += tempDef
    pendingLocalDefs += matchStmt
    tempVar

  // --- Match generation ----------------------------------------------

  private def genMatchExpr(
      selector: Tree, cases: List[CaseDef], pos: PyPosition, resultTpe: PyType
  ): PyTree =
    val sel = genExpr(selector)

    // Each case body must scope its own `pendingLocalDefs`. Otherwise
    // side-effecting genExpr calls inside a case (e.g. the `genExpr(Return)`
    // path pushes a `PyLabelReturn` as a pending stmt) would leak out of
    // the match and execute unconditionally before the dispatch runs.
    //
    // Same invariant as in `genExpr(If)`: a `PyMatch` returned from
    // `genExpr` must not contain a `PyBlock` (or other statement-shaped
    // node) inside a case body, because the emitter renders such a
    // `PyMatch` as a chain of Python conditional expressions and would
    // silently drop the block's statements. When any arm produced
    // pending locals we hoist the whole `Match` to a statement-form
    // `PyMatch` that assigns each arm's value to a fresh temp.
    def scopedBody(body: Tree): (List[PyTree], PyTree) =
      withLocalDefScope(genExpr(body))

    val litCases = mutable.ListBuffer.empty[(List[PyMatchableLiteral], List[PyTree], PyTree)]
    var defaultLocals: List[PyTree] = Nil
    var defaultExpr: PyTree = PyUnitLit()(pos)
    var defaultSet = false

    for caseDef <- cases do
      caseDef match
        case CaseDef(Literal(c), _, body) =>
          val lit = genLiteral(c, pos) match
            case ml: PyMatchableLiteral => ml
            case _ => PyNullLit()(pos): PyMatchableLiteral
          val (locals, expr) = scopedBody(body)
          litCases += ((List(lit), locals, expr))
        case CaseDef(_, _, body) =>
          if !defaultSet then
            val (locals, expr) = scopedBody(body)
            defaultLocals = locals
            defaultExpr = expr
            defaultSet = true

    val anyHasLocals =
      defaultLocals.nonEmpty || litCases.exists { case (_, ls, _) => ls.nonEmpty }
    if anyHasLocals then
      hoistValueMatch(sel, litCases.toList, defaultLocals, defaultExpr, resultTpe, pos)
    else
      val plainCases = litCases.toList.map { case (lits, _, expr) => (lits, expr) }
      PyMatch(sel, plainCases, defaultExpr)(resultTpe, pos)

  // --- Closure generation --------------------------------------------

  /** Emit a Scala closure as a Python lambda that forwards to the
   *  erasure-synthesised target method.
   *
   *  A `Closure(env, meth, tpt)` node produced by the erasure phase says:
   *  "build an instance of the SAM type `tpt.tpe` whose abstract method,
   *  when invoked, calls `meth(env..., args...)`."
   *
   *  We model this as:
   *    PyClosure(
   *      params = <one PyParamDef per SAM-method parameter>,
   *      body   = <static call to `meth` with env + the SAM params>)
   *
   *  The emitter renders this as `(lambda p0, p1, ...: owner.meth(e0, e1, ..., p0, p1, ...))`;
   *  Python's lexical scoping handles capture of `env` values inside
   *  `body` (e.g. the `genExpr(qual)` receiver for an instance target).
   */
  private def genClosure(tree: Closure): PyTree =
    val pos = posOf(tree)
    val targetSym = tree.meth.symbol
    val methodName = encoding.encodeMethodName(targetSym)
    val ownerClass = encoding.encodeClassName(targetSym.owner)
    val resultTpe = encoding.encodeType(targetSym.info.finalResultType)

    val isStaticTarget = isStaticMember(targetSym) && !isAnonfunDemotedFromStatic(targetSym)

    val targetParamTypes = targetSym.info.paramInfoss.flatten
    val envValues = tree.env.map(genExpr)
    val samParamCount = targetParamTypes.length - envValues.length
    val samParamInfos = targetParamTypes.drop(envValues.length)

    val samParams: List[PyParamDef] = samParamInfos.zipWithIndex.map { case (tpe, i) =>
      PyParamDef(
        name         = PyLocalName(s"_scpy_samarg_$i"),
        originalName = PyOriginalName.NoOriginalName,
        ptpe         = encoding.encodeType(tpe),
        mutable      = false,
        pos          = pos
      )
    }

    // For primitive-typed SAM parameters, the target body expects an
    // unboxed value (`int`, `float`, `bool`, ...) but the SAM dispatch
    // path can still pass `None` when the call site goes through the
    // boxed `apply(Object): Object` bridge with a `null` argument
    // (e.g. `genericCall1(if1_specialized)` where the source is
    // `foo(null.asInstanceOf[A])`). On the JVM the specialization
    // bridge unboxes `null -> 0` before forwarding. We replicate that
    // by wrapping each primitive samarg ref in `_scpy_unbox_or_default`,
    // which is a no-op when the actual value is already a primitive
    // and substitutes the primitive default when the value is `None`.
    // Non-primitive (Object/class-typed) target params bypass the
    // wrapper entirely so reference nulls still propagate.
    val samArgRefs: List[PyTree] = samParams.map { p =>
      val ref = PyVarRef(p.name)(p.ptpe, pos)
      primitiveTagOf(p.ptpe) match
        case Some(tag) =>
          PyApplyExternal(
            PyExternalName("_scpy_unbox_or_default"),
            List(PyStringLit(tag)(pos), ref)
          )(p.ptpe, pos)
        case None =>
          ref
    }
    val callArgs: List[PyTree] = envValues ++ samArgRefs

    val body: PyTree =
      if isStaticTarget then
        PyApplyStatic(
          PyApplyFlags.empty,
          ownerClass,
          methodName,
          callArgs
        )(resultTpe, pos)
      else if targetSym.owner.is(ModuleClass) then
        PyApply(
          PyApplyFlags.empty,
          PyDispatch.Virtual,
          moduleReceiver(targetSym.owner, pos),
          ownerClass,
          methodName,
          callArgs
        )(resultTpe, pos)
      else
        // Instance-method target: env values fill the first env.length
        // *parameters* of the target method (Trees.scala:594-606); the
        // receiver is encoded in `tree.meth`, typically `Select(qual, _)`
        // where `qual` is the captured `this`. A bare `Ident` means the
        // target is on the enclosing class with the qualifier elided.
        val receiver: PyTree = tree.meth match
          case Select(qual, _) => genExpr(qual)
          case _ =>
            PyThis()(PyClassType(encoding.encodeClassName(currentClassSym)), pos)
        PyApply(
          PyApplyFlags.empty,
          PyDispatch.Virtual,
          receiver,
          ownerClass,
          methodName,
          callArgs
        )(resultTpe, pos)

    PyClosure(
      params     = samParams,
      resultType = resultTpe,
      body       = body
    )(pos)

  /** If `tpe` is one of the eight JVM primitive value types, return its
   *  one-letter tag (matching `PyPrimRef.Tag.encoded`). Used by
   *  `genClosure` to decide whether to wrap a SAM parameter in
   *  `_scpy_unbox_or_default` so that a `null`/`None` arriving via the
   *  boxed `apply(Object)` bridge becomes the primitive default. */
  private def primitiveTagOf(tpe: PyType): Option[String] = tpe match
    case PyIntType     => Some("I")
    case PyLongType    => Some("J")
    case PyShortType   => Some("S")
    case PyByteType    => Some("B")
    case PyCharType    => Some("C")
    case PyFloatType   => Some("F")
    case PyDoubleType  => Some("D")
    case PyBooleanType => Some("Z")
    case _             => None

  private def moduleReceiver(moduleClass: Symbol, pos: PyPosition): PyTree =
    // Static methods (Python `@staticmethod`) have no `self` binding, so
    // even when the call target lives on the same module class we cannot
    // emit `PyThis()` from inside the body. Resolve through the module
    // singleton via `PyLoadModule(currentClassSym)` instead. Demoted
    // anonfuns (see `anonfunDemotedToInstance`) keep their `self` because
    // they are emitted as instance methods, so they are excluded from the
    // re-routing. This guards against the empty-package extension-method
    // shape exercised by `tests/run/for-desugar-strawman.scala`, where a
    // top-level `extension`-derived anonfun on a synthetic
    // `<file>$package$` module class calls a sibling static helper on the
    // same module — without the guard the call lowered to `self.method(…)`
    // and Python raised `NameError: name 'self' is not defined`.
    val inStaticContext =
      currentMethodSym != null && currentMethodSym.exists
        && (currentMethodSym.is(JavaStatic) || currentMethodSym.isScalaStatic)
        && !isAnonfunDemotedFromStatic(currentMethodSym)
    if moduleClass == currentClassSym && !inStaticContext then
      PyThis()(PyClassType(encoding.encodeClassName(currentClassSym)), pos)
    else
      PyLoadModule(encoding.encodeClassName(moduleClass))(pos)

  // --- File output ---------------------------------------------------

  /** Compute the `.pyir` filename for this CU.
    *
    *  Format: `<package>.<sourceName>.pyir` when all generated classes share
    *  a package; `<sourceName>.pyir` when there are no generated classes or
    *  they all live at the top level. If a CU emits classes spanning multiple
    *  packages (rare in Scala but legal), we drop the prefix and log an
    *  informational note - the former behaviour silently picked whichever
    *  class was emitted first and produced a nondeterministic filename.
    *
    *  The package is derived from each `PyClassDef`'s own encoded name
    *  rather than from the CU's `PackageDef`, so it stays in lockstep with
    *  the name scheme used throughout the PyIR/linker pipeline.
    *
    *  The cross-package note uses `report.echo` rather than `report.warning`
    *  so that user code that opts into `-Werror` (e.g. `i13215.scala`'s
    *  `//> using options -Werror -WunstableInlineAccessors` directive)
    *  doesn't get its compile elevated to an error by what is purely a
    *  diagnostic about backend filename selection.
    */
  private def deriveIrFileName(sourceName: String): String =
    def packagePrefixOf(fullName: String): Option[String] =
      fullName.lastIndexOf('.') match
        case -1  => None
        case idx => Some(fullName.substring(0, idx))

    val prefixes = generatedClasses.iterator
      .map(cls => packagePrefixOf(cls.name.nameString))
      .toSet

    val packagePrefix =
      prefixes.size match
        case 0 => None
        case 1 => prefixes.head
        case _ =>
          val distinct = prefixes.iterator
            .map(_.getOrElse("<root>"))
            .toList
            .sorted
            .mkString(", ")
          report.echo(
            s"ScalaPy: compilation unit ${genCtx.compilationUnit.source.file.name} " +
              s"emits classes across multiple packages ($distinct); " +
              "falling back to un-prefixed PyIR filename.",
            NoSourcePosition
          )
          None

    packagePrefix match
      case Some(pkg) => s"$pkg.$sourceName${PyIRFormat.FileExtension}"
      case None      => sourceName + PyIRFormat.FileExtension

  private def sourcePosOf(pos: PyPosition): SourcePosition =
    if pos.isEmpty then NoSourcePosition
    else
      val source = genCtx.compilationUnit.source
      if source.path != pos.source then NoSourcePosition
      else
        source.lineToOffsetOpt(pos.line) match
          case Some(lineOffset) =>
            val offset = (lineOffset + pos.column).max(0).min(source.length)
            source.atSpan(Span(offset))
          case None =>
            NoSourcePosition

  // --- Helpers -------------------------------------------------------

  private def posOf(tree: Tree): PyPosition =
    val pos = tree.sourcePos
    if pos.exists then PyPosition(pos.source.path, pos.line, pos.column)
    else PyPosition.NoPosition

  private def qualifierOf(tree: Tree): Tree = tree match
    case Select(qualifier, _) => qualifier
    case TypeApply(inner, _)  => qualifierOf(inner)
    case _                    => EmptyTree

  private def genExternRef(binding: ExternBinding, tp: Type, pos: PyPosition): PyExternalRef =
    PyExternalRef(binding.module, binding.path)(encoding.encodeType(tp), pos)

  private def genExternCall(sym: Symbol, args: List[Tree], pos: PyPosition): PyTree =
    val binding = encoding.externBindingOf(sym).get
    PyApplyDynamic(
      genExternRef(binding, sym.info.finalResultType, pos),
      genArgsPreservingOrder(args),
      Nil
    )(encoding.encodeType(sym.info.finalResultType), pos)

  private def genFacadeSelect(sel: Select, pos: PyPosition): PyTree =
    // Chained facade rebinding: if the selected member has its own @extern
    // binding, produce a fresh PyExternalRef from that binding instead of
    // extending the qualifier's path. This lets nested @extern objects
    // resolve to their declared Python module even when accessed through
    // an enclosing facade whose module is different (e.g. `np.linalg`
    // where `linalg` is rebound to `scipy.linalg`). For the same-module
    // case the rebinding is a no-op because the extension would produce
    // the same PyExternalRef.
    encoding.externBindingOf(sel.symbol) match
      case Some(binding) =>
        genExternRef(binding, sel.tpe, pos)
      case None =>
        val memberName = encoding.externMemberNameOf(sel.symbol)
        genDynamicSelect(
          genExpr(sel.qualifier),
          PyStringLit(memberName)(pos),
          Some(memberName),
          encoding.encodeType(sel.tpe),
          pos
        )

  private def genFacadeCall(sel: Select, args: List[Tree], pos: PyPosition): PyTree =
    PyApplyDynamic(
      genFacadeSelect(sel, pos),
      genArgsPreservingOrder(args),
      Nil
    )(encoding.encodeType(sel.symbol.info.finalResultType), pos)

  private def genDynamicApply(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if matchesPySymbol(sym, pyDefn.PyDynamic_selectDynamic) then
      app.args match
        case nameArg :: Nil =>
          val nameExpr = genExpr(nameArg)
          Some(genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            nameExpr,
            literalString(nameArg),
            encoding.encodeType(app.tpe),
            pos
          ))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_applyDynamic) then
      app.args match
        case nameArg :: dynArgs :: Nil =>
          val callee = genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            literalString(nameArg),
            PyAnyType,
            pos
          )
          Some(PyApplyDynamic(
            callee,
            genArgsPreservingOrder(extractRepeatedArgs(dynArgs)),
            Nil
          )(encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_applyDynamicNamed) then
      app.args match
        case nameArg :: kwargsArg :: Nil =>
          Some(genApplyDynamicNamedCall(app, nameArg, kwargsArg, pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_updateDynamic) then
      app.args match
        case nameArg :: valueArg :: Nil =>
          Some(genDynamicSetAttr(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            genExpr(valueArg),
            pos
          ))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.DynamicModule_module) then
      app.args match
        case moduleArg :: Nil =>
          Some(genDynamicModuleRef(moduleArg, encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.DynamicModule_attr) then
      app.args match
        case pathArg :: Nil =>
          Some(genDynamicBuiltinsAttr(pathArg, encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else None

  private def genDynamicSelect(
      receiver: PyTree,
      nameExpr: PyTree,
      literalName: Option[String],
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    literalName match
      case Some(name) if encoding.isValidPyAttrName(name) =>
        receiver match
          case ref: PyExternalRef =>
            PyExternalRef(ref.module, ref.path :+ name)(resultTpe, pos)
          case _ =>
            PyAttrAccess(receiver, name)(resultTpe, pos)
      case _ =>
        genGetAttr(receiver, nameExpr, resultTpe, pos)

  private def genGetAttr(
      receiver: PyTree,
      nameExpr: PyTree,
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    PyApplyDynamic(
      PyExternalRef("builtins", List("getattr"))(PyAnyType, pos),
      List(receiver, nameExpr),
      Nil
    )(resultTpe, pos)

  /** Emit an attribute assignment. Always lowered to `setattr(obj, name, value)`
   *  so keyword names and non-literal names work uniformly. */
  private def genDynamicSetAttr(
      receiver: PyTree,
      nameExpr: PyTree,
      value: PyTree,
      pos: PyPosition
  ): PyTree =
    PyApplyDynamic(
      PyExternalRef("builtins", List("setattr"))(PyAnyType, pos),
      List(receiver, nameExpr, value),
      Nil
    )(PyVoidType, pos)

  /** Lower a `d.applyDynamicNamed(methodName)(pairs*)` call where each
   *  pair is a `(String, Any)` tuple. Scala desugars both named-argument
   *  calls (`d.foo(k=v)`) and mixed positional+named calls
   *  (`d.foo(x, k=v)`) to this shape, with positional arguments getting
   *  empty-string names as the sentinel.
   *
   *  Pairs with an empty-string name become positional arguments of the
   *  emitted `PyApplyDynamic`; pairs with a non-empty name become keyword
   *  arguments. Runtime-computed keyword names and names that are not
   *  valid Python identifiers are rejected with a compile error — Python
   *  keyword-argument syntax requires static identifiers. */
  private def genApplyDynamicNamedCall(
      app: Apply,
      nameArg: Tree,
      kwargsArg: Tree,
      pos: PyPosition
  ): PyTree =
    val rawPairs = extractRepeatedArgs(kwargsArg)
    val parsed = rawPairs.map(extractKwargPair)
    val firstMissing = parsed.indexWhere(_.isEmpty)
    if firstMissing >= 0 then
      report.error(
        "scala.python.PyDynamic.applyDynamicNamed requires literal-string keyword " +
          "names; runtime-computed names are not supported.",
        rawPairs(firstMissing).sourcePos
      )
      PyUnitLit()(pos)
    else
      val resolved = parsed.flatten
      val invalidKw = resolved.find { case (name, _) =>
        name.nonEmpty && !encoding.isValidPyAttrName(name)
      }
      invalidKw match
        case Some((name, _)) =>
          report.error(
            s"Python keyword-argument name '$name' is not a valid identifier. " +
              "Python named-call syntax requires identifiers that are not reserved words.",
            app.sourcePos
          )
          PyUnitLit()(pos)
        case None =>
          val callee = genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            literalString(nameArg),
            PyAnyType,
            pos
          )
          val (positional, keyword) = resolved.partition { case (name, _) => name.isEmpty }
          val posArgs = positional.map { case (_, valueTree) => genExpr(valueTree) }
          val kwPairs = keyword.map { case (name, valueTree) => (name, genExpr(valueTree)) }
          PyApplyDynamic(callee, posArgs, kwPairs)(encoding.encodeType(app.tpe), pos)

  /** Unwrap a `(String, Any)` tuple construction tree to its name and value
   *  trees. Returns `None` if the first element is not a string literal.
   *
   *  Post-erasure, Scala tuple constructions appear as `Tuple2.apply(k, v)`
   *  or `new Tuple2(k, v)`. We match any 2-argument `Apply` whose first
   *  argument resolves to a literal string — in the context of
   *  `applyDynamicNamed`'s varargs, the only trees that can reach this
   *  matcher are `Tuple2` constructions anyway, so the permissive form
   *  is safe. */
  private def extractKwargPair(tree: Tree): Option[(String, Tree)] =
    tree match
      case Apply(_, List(nameArg, valueArg)) =>
        literalString(nameArg).map(_ -> valueArg)
      case Typed(inner, _)       => extractKwargPair(inner)
      case Inlined(_, Nil, expr) => extractKwargPair(expr)
      case Block(Nil, expr)      => extractKwargPair(expr)
      case _                     => None

  private def genDynamicModuleRef(moduleArg: Tree, resultTpe: PyType, pos: PyPosition): PyTree =
    literalString(moduleArg) match
      case Some(moduleName) =>
        PyExternalRef(moduleName, Nil)(resultTpe, pos)
      case None =>
        PyApplyDynamic(
          PyExternalRef("importlib", List("import_module"))(PyAnyType, pos),
          List(genExpr(moduleArg)),
          Nil
        )(resultTpe, pos)

  private def genDynamicBuiltinsAttr(pathArg: Tree, resultTpe: PyType, pos: PyPosition): PyTree =
    literalString(pathArg) match
      case Some(path) =>
        PyExternalRef("builtins", path.split('.').toList.filter(_.nonEmpty))(resultTpe, pos)
      case None =>
        genGetAttr(
          PyExternalRef("builtins", Nil)(PyAnyType, pos),
          genExpr(pathArg),
          resultTpe,
          pos
        )

  private def extractRepeatedArgs(tree: Tree): List[Tree] =
    tree match
      case seq: JavaSeqLiteral =>
        seq.elems
      case Apply(_, List(arg)) =>
        extractRepeatedArgs(arg)
      case Typed(inner, _) =>
        extractRepeatedArgs(inner)
      case Block(Nil, expr) =>
        extractRepeatedArgs(expr)
      case Inlined(_, Nil, expr) =>
        extractRepeatedArgs(expr)
      case other =>
        List(other)

  private def matchesPySymbol(sym: Symbol, expected: Symbol): Boolean =
    sym.exists && sym == expected

  private def literalString(tree: Tree): Option[String] =
    tree match
      case Literal(Constant(value: String)) =>
        Some(value)
      case Typed(inner, _) =>
        literalString(inner)
      case Block(Nil, expr) =>
        literalString(expr)
      case Inlined(_, Nil, expr) =>
        literalString(expr)
      case _ =>
        None

  private def collectMemberDefs(td: TypeDef): List[ValOrDefDef] =
    val impl = td.rhs.asInstanceOf[Template]
    val b = List.newBuilder[ValOrDefDef]
    for stat <- impl.constr :: impl.body do
      stat match
        case stat: ValDef => b += stat
        case stat: DefDef => b += stat
        case _            => ()
    b.result()

  private def isStaticModule(sym: Symbol): Boolean =
    sym.is(ModuleClass) && !sym.isAnonymousClass

  /** Flush `pendingLocalDefs` and flatten nested `PyBlock`s. */
  private def flattenToStmts(tree: PyTree): List[PyTree] =
    val prefix = pendingLocalDefs.toList
    pendingLocalDefs.clear()
    val flat: List[PyTree] = tree match
      case PyBlock(stats, expr) =>
        stats.flatMap(flattenToStmts) ::: flattenToStmts(expr)
      case _: PyUnitLit => Nil
      case _: PySkip    => Nil
      case other        => List(other)
    prefix ::: flat
end PyCodeGen

// Pure helpers used by `PyCodeGen`; factored out so unit tests can drive
// them without instantiating the full backend phase.
private[python] object PyCodeGenSupport:

  // Pick the bundle's main entry from every `@main`-bearing class found
  // in this CU, deterministically.
  //
  // The previous implementation overwrote `mainEntry` on each match in
  // the traversal loop, so whichever `@main`-bearing class arrived last
  // from the pipeline won. Across runs this could flip - for example
  // `tests/run/main-functions.scala` (with `@main def Test` at top level
  // and `@main def foo` inside `object A`) sometimes selected `Test` and
  // sometimes `foo`.
  //
  // Selection rule (deterministic, total order over candidate names):
  //   1. If `explicitName` is non-empty (typically `-Xmain-class`) and
  //      matches a candidate's full name, use it (matches the JVM
  //      backend's manifest override).
  //   2. Otherwise, sort candidates by their encoded name string and
  //      pick the first. When there is more than one candidate this
  //      also calls `onAmbiguity` so the disambiguation is visible.
  //
  // The sort uses `PyClassName.nameString` directly: it is the encoded
  // full class name (e.g. `Test`, `foo`, `pkg.Main`) and is part of the
  // serialized `.pyir` contract, so the chosen entry is stable across
  // runs and across machines for a given source. `Test` sorts before
  // `foo` (capital letters precede lowercase in ASCII), which matches
  // the convention vulpix uses for `tests/run/*` (it invokes
  // `Test.main`).
  def pickMainEntry(
      candidates: List[PyIREmitter.MainEntry],
      explicitName: String,
      onAmbiguity: (List[String], String) => Unit
  ): Option[PyIREmitter.MainEntry] =
    candidates match
      case Nil      => None
      case c :: Nil => Some(c)
      case many =>
        val sorted = many.sortBy(_._1.nameString)
        val explicit =
          if explicitName.isEmpty then None
          else sorted.find(_._1.nameString == explicitName)
        val chosen = explicit.getOrElse(sorted.head)
        onAmbiguity(sorted.map(_._1.nameString), chosen._1.nameString)
        Some(chosen)
end PyCodeGenSupport
