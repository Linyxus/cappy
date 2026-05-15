package dotty.tools.cappyrepl

import java.io.{ByteArrayOutputStream, PrintStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.language.unsafeNulls
import scala.util.control.NonFatal

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.reporting.{Diagnostic, MessageRendering, StoreReporter}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.VirtualDirectory
import dotty.tools.backend.python.{
  GenPython, PyClasspathLoader, PyIREmitter, PyIRRuntime, PyLinker
}
import dotty.tools.backend.python.ir.pyir.PyClassDef

/** Headless Cappy REPL driver.
 *
 *  Owns a persistent `uv run python` subprocess and an incremental dotc
 *  compile pipeline. Per [[run]]:
 *
 *    1. Parse + wrap as `object cappy_line_N { val resK = ... ; ... }`
 *       (via [[CappyReplCompiler]]).
 *    2. Run dotc; GenPython sees a `PyReplSink` on the Context and
 *       hands the freshly built `PyClassDef`s back here instead of
 *       writing `.pyir`/`.py` to disk.
 *    3. Link with `LinkMode.ReplIncrement` against the cached support
 *       inputs — the bundle contains only the new user class(es).
 *    4. Emit with `includePreamble = false` / `includeClosureCarriers = false`
 *       — the subprocess already has those from startup.
 *    5. Append `_cappy_render_*` calls produced by [[CappyRendering]] so
 *       the live Python values are formatted as `val x: Int = 5`.
 *    6. `process.send(...)` pipes the chunk to Python and reads back
 *       everything printed until the end-marker.
 *
 *  Constructor parameters:
 *  @param settings  Extra compiler args (added after the built-ins).
 *  @param out       Where Python subprocess output is forwarded. Tests
 *                   capture this with a `ByteArrayOutputStream`.
 *  @param classpath Colon-separated classpath. Defaults to the
 *                   `cappy.classpath` system property set by sbt.
 *  @param repoRoot  uv project root. Defaults to `cappy.repoRoot`.
 */
class CappyReplDriver(
    settings:    Array[String] = Array.empty,
    val out:     PrintStream   = System.out,
    classpath:   String        = System.getProperty("cappy.classpath", ""),
    repoRoot:    String        = System.getProperty("cappy.repoRoot", ".")
) extends Driver:

  override def sourcesRequired: Boolean = false

  private var rootCtx: Context        = uninitialized
  private var compiler: CappyReplCompiler = uninitialized
  private var process: PythonProcess  = uninitialized
  private var supportInputs: List[PyLinker.Input] = Nil
  private var initialized: Boolean    = false

  // Every PyClassDef whose Python definition has already been sent to
  // the live subprocess, keyed by class name. Per-input emission filters
  // bundle classes against this map so classes that have shown up before
  // aren't redefined (which would clobber prior instances).
  //
  // Past wrapper classes (e.g. `cappy_line_1_`) are ALSO supplied to
  // subsequent links as a phantom `Support` input — `cappy_line_2`'s
  // reference to `cappy_line_1.x` would otherwise fail the linker's
  // nominal validation.
  private val emittedClasses = mutable.LinkedHashMap.empty[
    dotty.tools.backend.python.ir.pyir.PyClassName,
    PyClassDef
  ]
  private var closureCarriersEmitted: Boolean = false

  // Output dir is a VirtualDirectory so each compile's class/tasty files
  // live in memory and survive across runs — the typer can resolve
  // `cappy_line_<K>` references from prior inputs.
  private val virtualOutputDir = new VirtualDirectory("<cappy-repl>")

  // Captured PyClassDef batches from the GenPython sink. Cleared at the
  // start of each `run` call.
  private val sinkBuffer = mutable.ListBuffer.empty[(String, List[PyClassDef], Option[PyIREmitter.MainEntry])]
  private val sink: GenPython.PyReplSink =
    new GenPython.PyReplSink:
      def onCompiled(
          sourceName: String,
          classes:    List[PyClassDef],
          mainEntry:  Option[PyIREmitter.MainEntry]
      ): Unit =
        sinkBuffer += ((sourceName, classes, mainEntry))

  // -------------------------------------------------------------------
  // Lifecycle
  // -------------------------------------------------------------------

  /** Build the root compile context from settings + injected classpath. */
  private def initialCtx(): Context =
    val baseSettings = Array(
      "-scalapy",
      "-color:never",
      "-classpath", classpath,
    )
    val all = baseSettings ++ settings
    val rc = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    setup(all, rc) match
      case Some((_, ictx)) =>
        // Pin the output dir to our VirtualDirectory so wrappers stay
        // in memory across runs and the next compile sees them on the
        // classpath.
        val withOut = ictx.fresh.setSetting(ictx.settings.outputDir, virtualOutputDir)
        // Install the PyReplSink so GenPython hands classes back via
        // the in-memory callback (and skips its disk write).
        withOut.setProperty(GenPython.ReplSinkKey, sink)
        withOut.base.initialize()(using withOut)
        withOut
      case None =>
        rc

  /** Reset to a fresh REPL session. Re-launches the subprocess and the
   *  compiler. Sends the runtime preamble + support bundle to the new
   *  Python process. */
  def resetToInitial(): Unit =
    if process != null && process.isAlive then process.shutdown()
    sinkBuffer.clear()
    emittedClasses.clear()
    closureCarriersEmitted = false
    rootCtx = initialCtx()
    compiler = new CappyReplCompiler
    process = PythonProcess.start(repoRoot)
    supportInputs = loadSupportInputs(rootCtx)
    sendStartupBundle()
    initialized = true

  private def loadSupportInputs(ctx: Context): List[PyLinker.Input] =
    PyClasspathLoader.loadSupportInputs(excludeOutputFile = None)(using ctx)

  /** Send the runtime preamble + REPL-side render helpers at startup.
   *
   *  Note: we deliberately do **not** pre-emit support classes here.
   *  Forcing the support surface "all reachable" exposes references to
   *  runtime-provided classes that the linker can't resolve outside the
   *  normal DCE-driven walk (e.g. `scala.runtime.StructuralCallSite` ->
   *  `java.lang.invoke.ConstantCallSite`). Instead each REPL input is
   *  linked normally and the driver tracks which support classes have
   *  already been sent, emitting only those new on this turn. */
  private def sendStartupBundle(): Unit =
    val preamble = PyIRRuntime.content
    process.send(preamble + "\n" + replHelpersPython)

  /** Build the fresh initial State (objectIndex = 0). The first user
   *  input produces `cappy_line_1`. */
  def initialState: CappyReplState =
    if !initialized then resetToInitial()
    CappyReplState(
      objectIndex          = 0,
      valIndex             = 0,
      imports              = Map.empty,
      invalidObjectIndexes = Set.empty,
      context              = rootCtx
    )

  /** Shut down the Python subprocess. Subsequent `run` calls error. */
  def shutdown(): Unit =
    if process != null then
      process.shutdown()
      process = null

  // -------------------------------------------------------------------
  // Per-input entry point
  // -------------------------------------------------------------------

  /** Compile + evaluate `input` and return the output the Python
   *  subprocess emitted (rendered bindings, user `println`s, etc.).
   *
   *  Compiler errors are formatted into the return string; the REPL
   *  state advances only on success. The subprocess remains alive
   *  either way. */
  def run(input: String)(using state: CappyReplState): (String, CappyReplState) =
    if !initialized then resetToInitial()
    sinkBuffer.clear()

    val source = SourceFile.virtual(s"cappy_line_${state.objectIndex + 1}.scala", input)
    val reporter = new StoreReporter(null)
    val runCtx = compiler.newRun(rootCtx.fresh.setReporter(reporter), state).runContext

    given freshState: CappyReplState = state.copy(context = runCtx)

    try
      compiler.compile(input, source) match
        case Left(errs) =>
          (formatErrors(errs, runCtx), state)

        case Right((unit, newState)) =>
          val wrapperPyClass = sinkBuffer.toList match
            case Nil =>
              // Nothing emitted: e.g. user typed only an import or
              // whitespace. Still bump state.
              return ("", newState.copy(context = runCtx))
            case batches =>
              // The wrapper is the module class of `cappy_line_<idx>`,
              // i.e. the PyClassDef whose name's last segment ends with
              // an underscore (the trailing `$` of Scala's module class
              // gets sanitized to `_`).
              val allClasses = batches.flatMap(_._2)
              allClasses
                .find(c => c.name.nameString.startsWith(CappyReplCompiler.WrapperPrefix))
                .getOrElse(allClasses.head)

          val allClasses    = sinkBuffer.toList.flatMap(_._2)
          val mainEntry     = sinkBuffer.toList.flatMap(_._3).headOption

          // Link normally (Bundle); filter to "classes not yet emitted"
          // afterwards so support classes the subprocess already has
          // aren't redefined.
          val userInput = PyLinker.Input(
            classes   = allClasses,
            mainEntry = mainEntry,
            source    = PyLinker.InputSource.User
          )
          // Include every class we've already emitted as a phantom
          // Support input. This satisfies the linker's nominal-reference
          // check when this input references a prior wrapper.
          val phantomSupport: List[PyLinker.Input] =
            if emittedClasses.isEmpty then Nil
            else List(PyLinker.Input(
              classes   = emittedClasses.values.toList,
              mainEntry = None,
              source    = PyLinker.InputSource.Support
            ))

          val bundle =
            try PyLinker.link(
              userInputs    = List(userInput),
              supportInputs = supportInputs ++ phantomSupport,
              mode          = PyLinker.LinkMode.ReplIncrement
            )
            catch case ex: dotty.tools.backend.python.PyLinkingException =>
              return (s"link error:\n${ex.getMessage}", state)

          val newClasses = bundle.classes.filterNot(c => emittedClasses.contains(c.name))
          // Closure carriers reference `scala.FunctionN` classes. Emit
          // them once on the first increment that brings any FunctionN
          // arity in, then never again — re-emitting would shadow
          // `_scpy_FnN` instances captured by closures handed out by
          // earlier inputs.
          val incomingFunctionN = newClasses.exists(c =>
            c.name.nameString.startsWith("scala.Function")
            && c.name.nameString.drop("scala.Function".length).forall(_.isDigit)
          )
          val needsCarriers = incomingFunctionN && !closureCarriersEmitted
          if needsCarriers then closureCarriersEmitted = true

          val emittedPy = PyIREmitter.emitToString(
            newClasses,
            mainEntry              = None,
            includePreamble        = false,
            includeClosureCarriers = needsCarriers
          )
          for c <- newClasses do emittedClasses(c.name) = c

          // Locate the wrapper symbol so rendering can read its info.
          val wrapperSym = findWrapperSymbol(unit.tpdTree, newState.objectIndex)(using runCtx)
          val renderPy =
            wrapperSym match
              case Some(sym) =>
                CappyRendering.renderBindings(wrapperPyClass, sym)(using runCtx)
              case None => ""

          val output =
            try process.send(emittedPy + "\n" + renderPy)
            catch case ex: PythonProcessTerminated => ex.getMessage

          (output, newState.copy(context = runCtx))
    catch case NonFatal(ex) =>
      val sw = new StringWriter
      ex.printStackTrace(new PrintWriter(sw))
      (s"internal error: ${ex.getClass.getName}: ${ex.getMessage}\n${sw.toString}", state)

  /** Convenience: `run` and write the captured output to `out`. */
  def runAndPrint(input: String)(using state: CappyReplState): CappyReplState =
    val (output, newState) = run(input)
    if output.nonEmpty then out.println(output)
    newState

  private def findWrapperSymbol(tpdTree: tpd.Tree, objectIndex: Int)(using Context): Option[Symbol] =
    val wrapperName = CappyReplCompiler.wrapperTermName(objectIndex)
    // After typer, an `object cappy_line_N { ... }` shows up as a
    // `TypeDef` whose symbol is the module class (Module + ModuleClass
    // flags). The module-class name has a trailing `$` in source form
    // but `show` strips it; we just match on the prefix.
    tpdTree match
      case pkg: tpd.PackageDef =>
        pkg.stats.iterator.collectFirst {
          case td: tpd.TypeDef
            if td.symbol.is(dotty.tools.dotc.core.Flags.ModuleClass)
            && td.name.show.stripSuffix("$") == wrapperName =>
            td.symbol
        }
      case _ => None

  private def formatErrors(errs: List[Diagnostic], ctx: Context): String =
    val mr = new MessageRendering {}
    val buf = new StringBuilder
    errs.foreach { d =>
      buf.append(mr.messageAndPos(d)(using ctx))
      buf.append('\n')
    }
    buf.toString.stripLineEnd

  // -------------------------------------------------------------------
  // Python-side helpers
  // -------------------------------------------------------------------

  /** A handful of small Python helpers sent to the subprocess at
   *  startup. They format binding lines using `_scpy_to_str` (already
   *  in the runtime preamble) and look up Scala-encoded accessor
   *  methods reflectively. */
  private def replHelpersPython: String =
    """|def _cappy_force_module(mod):
       |    # Touching `_scpy_module_value` runs the lazy module's __init__.
       |    try:
       |        _scpy_module_value(mod)
       |    except Exception as exc:
       |        print(f"<initialization failed: {type(exc).__name__}: {exc}>")
       |
       |def _cappy_lookup_accessor(mod, simple_name):
       |    # Scala val accessors compile to a Python method named
       |    # `<simple>__<paramRefs>__<resultRef>` (empty paramRefs for vals).
       |    # Find the no-arg accessor by simple-name prefix.
       |    inst = _scpy_module_value(mod)
       |    prefix = simple_name + '__'
       |    for n in dir(inst):
       |        if n == simple_name or n.startswith(prefix):
       |            attr = getattr(inst, n, None)
       |            if callable(attr):
       |                try:
       |                    return attr()
       |                except TypeError:
       |                    continue
       |            else:
       |                return attr
       |    return None
       |
       |def _cappy_render_val(dcl, mod, simple_name):
       |    try:
       |        v = _cappy_lookup_accessor(mod, simple_name)
       |    except Exception as exc:
       |        print(f"{dcl} = <error: {type(exc).__name__}: {exc}>")
       |        return
       |    if v is None:
       |        print(dcl)
       |    else:
       |        print(f"{dcl} = {_scpy_to_str(v)}")
       |
       |def _cappy_render_def(dcl):
       |    print(dcl)
       |
       |def _cappy_render_typedef(kind, name):
       |    print(f"// defined {kind} {name}")
       |""".stripMargin

end CappyReplDriver
