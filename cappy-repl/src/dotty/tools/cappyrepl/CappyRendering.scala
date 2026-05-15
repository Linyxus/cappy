package dotty.tools.cappyrepl

import scala.collection.mutable
import scala.language.unsafeNulls

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.{DefaultGetterName, SimpleNameKind}
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.printing.ReplPrinter
import dotty.tools.backend.python.ir.pyir.{PyClassDef, PyClassKind, PyClassName}

/** Post-compile rendering: walks the wrapper module's symbol and
 *  produces Python source that prints `val x: Int = 5` / `def foo: (...)`
 *  / `// defined class Foo` style lines.
 *
 *  Mirrors `dotty.tools.repl.Rendering.extractAndFormatMembers`. The
 *  major adaptation: scala3-repl uses JVM reflection to fetch live values
 *  out of its classloader. We can't — the values live in a separate
 *  Python interpreter — so we generate Python `_cappy_render_val(...)`
 *  calls that look up the accessor reflectively on the Python side.
 *
 *  The `_cappy_render_*` helpers themselves are sent to the subprocess
 *  at startup; see [[CappyReplDriver.replHelpersPython]].
 */
object CappyRendering:

  /** Build the Python rendering block for the latest wrapper.
   *
   *  Returns Python source (no trailing newline) that should be appended
   *  after the emitted user classes and sent to the subprocess.
   */
  def renderBindings(
      wrapperClass: PyClassDef,
      wrapperSym:   Symbol
  )(using Context): String =
    val moduleVar = moduleVarName(wrapperClass.name)

    val members = collectRenderableMembers(wrapperSym)
    val lines = mutable.ListBuffer.empty[String]

    // Force the module to initialize even if it has no renderable
    // members — matches scala3-repl's `forceModule`. Side effects
    // (println at top level) only fire when the singleton is accessed.
    lines += s"_cappy_force_module($moduleVar)"

    for m <- members do
      m match
        case Member.Val(name, dcl) =>
          lines += s"_cappy_render_val(${pyStr(dcl)}, $moduleVar, ${pyStr(name)})"
        case Member.Def(dcl) =>
          lines += s"_cappy_render_def(${pyStr(dcl)})"
        case Member.TypeDef(kind, name) =>
          lines += s"_cappy_render_typedef(${pyStr(kind)}, ${pyStr(name)})"

    lines.mkString("\n")

  // --- Renderable member taxonomy ------------------------------------

  private enum Member:
    case Val(simpleName: String, dcl: String)
    case Def(dcl: String)
    case TypeDef(kind: String, name: String)

  /** Walks the wrapper module's symbol and returns members worth showing
   *  to the user. Filters out synthetic, private, accessor-for-private,
   *  and constructor-style entries — same predicates as
   *  scala3-repl's Rendering. */
  private def collectRenderableMembers(wrapperSym: Symbol)(using Context): List[Member] =
    val info = wrapperSym.info
    val printer = new ReplPrinter(ctx)
    def show(sym: Symbol): String =
      printer.dclText(sym).mkString(ctx.settings.pageWidth.value).trim

    val out = mutable.ListBuffer.empty[Member]

    // Val accessors (`info.fields` returns the field denotations of the
    // refined-type members; for a Scala val this is the underlying field
    // backing the accessor method).
    val vals = info.fields
      .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
      .filter(_.symbol.name.is(SimpleNameKind))
    for d <- vals do
      val sym = d.symbol
      if sym.info != defn.UnitType then
        out += Member.Val(sym.name.show, show(sym))

    // Plain `def` members (non-constructor, non-default-getter, non-synthetic).
    val defs =
      info.bounds.hi.finalResultType
        .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
        .filterNot { d =>
          defn.topClasses.contains(d.symbol.owner)
          || d.symbol.isConstructor
          || d.symbol.name.is(DefaultGetterName)
        }
    for d <- defs do
      out += Member.Def(show(d.symbol))

    // Nested class/object definitions appear via memberClasses.
    val typeDefs = wrapperSym.info.memberClasses
    for d <- typeDefs do
      val ts = d.symbol
      if !ts.is(Synthetic) && !ts.name.toString.startsWith("$anon") then
        val kind = if ts.is(Module) then "object" else if ts.is(Trait) then "trait" else "class"
        out += Member.TypeDef(kind, ts.name.show.stripSuffix("$"))

    out.toList

  // --- Python identifier helpers (mirror PyIREmitter.moduleVarName) --

  /** Python variable holding the module singleton.
   *
   *  Mirrors `PyIREmitter.Emitter.moduleVarName` (which is private).
   *  Format: `_scpy_mod_<segments joined by _>_`. */
  def moduleVarName(cn: PyClassName): String =
    s"_scpy_mod_${cn.segments.map(sanitizeIdent).mkString("_")}_"

  /** Mirrors `PyIRRuntime.sanitizeIdent`: replace anything that is not a
   *  legal Python identifier char with `_`. Conservative for the
   *  segments we typically encounter (`cappy_line_<N>_`, stdlib FQNs). */
  private def sanitizeIdent(s: String): String =
    val buf = new StringBuilder(s.length)
    for c <- s do
      if c.isLetterOrDigit || c == '_' then buf += c
      else buf += '_'
    buf.toString

  /** Python string literal escaping. */
  private def pyStr(s: String): String =
    val buf = new StringBuilder(s.length + 2)
    buf += '"'
    for c <- s do
      c match
        case '\\' => buf ++= "\\\\"
        case '"'  => buf ++= "\\\""
        case '\n' => buf ++= "\\n"
        case '\r' => buf ++= "\\r"
        case '\t' => buf ++= "\\t"
        case c if c < 0x20 => buf ++= f"\\x${c.toInt}%02x"
        case c    => buf += c
    buf += '"'
    buf.toString
end CappyRendering
