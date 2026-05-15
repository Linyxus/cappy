package dotty.tools.cappyrepl

import scala.collection.mutable
import scala.language.unsafeNulls

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.{nme, str}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.transform.PostTyper
import dotty.tools.dotc.typer.{ImportInfo, TyperPhase}
import dotty.tools.dotc.typer.ImportInfo.{RootRef, withRootImports}
import dotty.tools.dotc.util.{Property, SourceFile, Spans}
import dotty.tools.backend.python.GenPython

/** Compiler used by `CappyReplDriver` for one REPL input.
 *
 *  Mirrors `dotty.tools.repl.ReplCompiler`: a `Parser` parses the input,
 *  the `ReplPhase` wraps the parsed statements in `object cappy_line_N`
 *  (rewriting free expressions as `val resN`), the typer + post-typer +
 *  backend then proceed normally. The Python backend's `GenPython` runs
 *  at the end and routes through the `PyReplSink` set on the Context
 *  (see [[CappyReplDriver.compileOne]]).
 *
 *  Per-input incremental scoping is handled in [[newRun]]: the rootContext
 *  imports every prior wrapper module so subsequent inputs see the
 *  definitions introduced earlier. This mirrors `ReplCompiler.newRun`.
 */
class CappyReplCompiler extends Compiler:

  // The real `dotty.tools.dotc.parsing.Parser` always re-parses
  // `unit.source` as a top-level compilation unit; that rejects bare
  // expressions like `1 + 1` with "Illegal start of toplevel definition".
  // Use a no-op placeholder so the pre-parsed tree we attached in
  // `compile()` (block-stat-seq + ReplState attachment) survives into
  // CappyReplPhase. Same trick scala3-repl uses (`repl.Parser`).
  //
  // `CappyCollectTopLevelImports` runs right after Typer to harvest
  // user-typed `import` statements from the wrapper module; the driver
  // re-applies them in subsequent runs' root context.
  override protected def frontendPhases: List[List[Phase]] = List(
    List(new CappyParserPlaceholder),
    List(new CappyReplPhase),
    List(new TyperPhase(addRootImports = false)),
    List(new CappyCollectTopLevelImports),
    List(new PostTyper),
  )

  /** Build a fresh Run whose root context imports every prior wrapper. */
  def newRun(initCtx: Context, state: CappyReplState): Run =
    val run = new Run(this, initCtx):
      override protected def rootContext(using Context): Context =
        // Standard root setup: empty package + root imports.
        val rootCtx = super.rootContext.fresh
          .withRootImports
          .fresh.setOwner(defn.EmptyPackageClass): Context
        state.validObjectIndexes.foldLeft(rootCtx)((c, i) =>
          importWrapper(i, state)(using c))
    run.suppressions.initSuspendedMessages(state.context.run)
    run

  /** Add `import cappy_line_<id>.*` to the given context, then re-apply
   *  every user-typed `import` we saw inside that wrapper (recovered
   *  by `CappyCollectTopLevelImports` at compile time and stored on
   *  the state). */
  private def importWrapper(id: Int, state: CappyReplState)(using Context): Context =
    val name = CappyReplCompiler.wrapperTermName(id)
    val path = nme.EMPTY_PACKAGE ++ "." ++ name
    val ctxWithWrapper = ctx.fresh
      .setNewScope
      .withRootImports(RootRef(() => requiredModuleRef(path)) :: Nil)
    val userImports = state.imports.getOrElse(id, Nil)
    if userImports.isEmpty then ctxWithWrapper
    else userImports.foldLeft(ctxWithWrapper.fresh.setNewScope) { (c, imp) =>
      c.importContext(imp, imp.symbol(using c))
    }

  /** Compile one input. The parsed trees are attached to the unit; the
   *  `CappyReplPhase` reads the [[CappyReplCompiler.ReplStateKey]]
   *  attachment to drive its wrapping. Returns either parser/typer
   *  diagnostics or `(unit, newState)`.
   */
  def compile(input: String, source: SourceFile)
             (using state: CappyReplState): Either[List[Diagnostic], (CompilationUnit, CappyReplState)] =
    given Context = state.context.fresh.setSource(source)
    val unit = CompilationUnit(source, mustExist = false)
    // Mirror scala3-repl's `parseStats`: parse the input as a sequence
    // of block statements (top-level expressions are valid). Wrap the
    // resulting trees in a placeholder `PackageDef(<empty>, stats)`
    // carrying the full input span; `CappyReplPhase` then rewraps them
    // in `object cappy_line_N { ... }`.
    val parser = new dotty.tools.dotc.parsing.Parsers.Parser(source)
    val stats = parser.blockStatSeq(outermost = true)
    parser.accept(dotty.tools.dotc.parsing.Tokens.EOF)

    val span = dotty.tools.dotc.util.Spans.Span(0, source.content().length)
    val parsed = untpd
      .PackageDef(untpd.Ident(nme.EMPTY_PACKAGE).withSpan(span), stats)
      .withSpan(span)
    parsed.putAttachment(CappyReplCompiler.ReplStateKey, state)
    unit.untpdTree = parsed
    ctx.run.nn.compileUnits(unit :: Nil)
    ctx.run.nn.printSummary()

    if ctx.reporter.hasErrors then
      Left(ctx.reporter.removeBufferedMessages)
    else
      val newState =
        Option(unit.tpdTree).flatMap(_.getAttachment(CappyReplCompiler.ReplStateKey)).getOrElse(state)
      Right(unit, newState)

object CappyReplCompiler:

  /** Per-input state attachment so `CappyReplPhase` can read the
   *  [[CappyReplState]] (specifically: `objectIndex`, `valIndex`) when
   *  wrapping the parsed trees. */
  val ReplStateKey: Property.StickyKey[CappyReplState] = new Property.StickyKey

  /** Wrapper line prefix. The N'th input becomes `object cappy_line_N`. */
  inline val WrapperPrefix = "cappy_line_"

  def wrapperTermName(id: Int): String = s"$WrapperPrefix$id"

/** No-op stand-in for the real Parser phase. The driver has already
 *  parsed the input via `Parsers.Parser.blockStatSeq` and attached the
 *  result to `unit.untpdTree`; this class exists only so dotc's pipeline
 *  has a phase called "parser" without trying to re-parse. */
final class CappyParserPlaceholder extends Phase:
  override def phaseName: String = "parser"
  override def run(using Context): Unit = ()
end CappyParserPlaceholder

/** Wraps each input's parsed top-level statements in
 *  `object cappy_line_<idx> { ... }`. Free expressions become
 *  `val res<valIndex> = <expr>`. The updated state is stashed back on
 *  the new tree so the driver can pick it up.
 */
final class CappyReplPhase extends Phase:
  override def phaseName: String = "cappyRepl"

  override def run(using Context): Unit =
    ctx.compilationUnit.untpdTree match
      case pkg: untpd.PackageDef =>
        pkg.getAttachment(CappyReplCompiler.ReplStateKey) match
          case Some(state) =>
            val (newStats, newState) = liftFreeExprs(pkg.stats, state)
            val wrapped = wrapInModule(newStats, newState, pkg.span)
            wrapped.putAttachment(CappyReplCompiler.ReplStateKey, newState)
            ctx.compilationUnit.untpdTree = wrapped
          case None => ()
      case _ => ()

  /** Lift free expressions to `val resN = <expr>`. Re-uses the
   *  scala3-repl pattern from ReplCompiler.ReplPhase.definitions. */
  private def liftFreeExprs(trees: List[untpd.Tree], state: CappyReplState)(using Context)
      : (List[untpd.Tree], CappyReplState) =
    val flattened = trees match
      case List(untpd.Block(stats, expr)) =>
        if expr eq untpd.EmptyTree then stats else stats :+ expr
      case _ => trees

    var valIdx = state.valIndex
    val out = mutable.ListBuffer.empty[untpd.Tree]
    flattened.foreach {
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val vd = untpd.ValDef(resName, untpd.TypeTree(), expr).withSpan(expr.span)
        out += vd
      case other =>
        out += other
    }
    val newState = state.copy(
      objectIndex = state.objectIndex + 1,
      valIndex    = valIdx
    )
    (out.toList, newState)

  private def wrapInModule(
      stats:    List[untpd.Tree],
      newState: CappyReplState,
      span:     Spans.Span
  )(using Context): untpd.PackageDef =
    import untpd.*
    val wrapperName = CappyReplCompiler.wrapperTermName(newState.objectIndex).toTermName
    val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, stats).withSpan(span)
    val mod = ModuleDef(wrapperName, tmpl).withSpan(span)
    val emptyPkg = Ident(nme.EMPTY_PACKAGE).withSpan(span)
    PackageDef(emptyPkg, List(mod)).withSpan(span)
end CappyReplPhase

/** Post-Typer phase: harvest user-typed `import` statements from the
 *  wrapper module's body so the driver can re-apply them in the next
 *  run's root context. Ported from `dotty.tools.repl.CollectTopLevelImports`. */
final class CappyCollectTopLevelImports extends Phase:
  import dotty.tools.dotc.ast.tpd.*

  override def phaseName: String = "cappyCollectTopLevelImports"

  private var myImports: List[Import] = Nil
  def imports: List[Import] = myImports

  override def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    myImports = tree match
      case PackageDef(_, stats) =>
        // The wrapper is an `object cappy_line_N`; post-typer expands
        // it to a `ValDef` (the module's term side) and a `TypeDef`
        // for the module class. Walk the module class's Template body
        // and collect every top-level `Import` tree.
        stats.flatMap {
          case td: TypeDef => td.rhs match
            case tmpl: Template => tmpl.body.collect { case imp: Import => imp }
            case _              => Nil
          case _ => Nil
        }
      case _ => Nil
end CappyCollectTopLevelImports
