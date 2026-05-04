package dotty.tools.dotc
package transform

import MegaPhase.*
import core.DenotTransformers.*
import core.Symbols.*
import core.Contexts.*
import core.Flags.*
import core.Decorators.*
import core.StdNames.nme
import core.Names.*
import core.NameKinds.TempResultName
import core.Constants.*
import util.Store
import dotty.tools.uncheckedNN
import ast.tpd.*
import compiletime.uninitialized

/** This phase translates variables that are captured in closures to
 *  heap-allocated refs.
 */
class CapturedVars extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>

  override def phaseName: String = CapturedVars.name

  override def description: String = CapturedVars.description

  private val captured = util.HashSet[Symbol]()

  private class RefInfo(using Context) {
    /** The classes for which a Ref type exists. */
    val refClassKeys: collection.Set[Symbol] =
      defn.ScalaNumericValueClasses() `union` Set(defn.BooleanClass, defn.ObjectClass)

    val refClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> requiredClass(s"scala.runtime.${rc.name}Ref")).toMap

    val volatileRefClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> requiredClass(s"scala.runtime.Volatile${rc.name}Ref")).toMap

    val boxedRefClasses: collection.Set[Symbol] =
      refClassKeys.flatMap(k => Set(refClass(k), volatileRefClass(k)))

    val objectRefClasses: collection.Set[Symbol] =
      Set(refClass(defn.ObjectClass), volatileRefClass(defn.ObjectClass))
  }

  private var myRefInfo: RefInfo | Null = null
  private def refInfo(using Context): RefInfo = {
    if (myRefInfo == null) myRefInfo = new RefInfo()
    myRefInfo.uncheckedNN
  }

  override def prepareForUnit(tree: Tree)(using Context): Context =
    captured.clear()
    atPhase(thisPhase)(CapturedVars.collect(captured)).traverse(tree)
    ctx

  /** The {Volatile|}{Int|Double|...|Object}Ref class corresponding to the class `cls`,
    *  depending on whether the reference should be @volatile
    */
  def refClass(cls: Symbol, isVolatile: Boolean)(using Context): Symbol = {
    val refMap = if (isVolatile) refInfo.volatileRefClass else refInfo.refClass
    if (cls.isClass)
      refMap.getOrElse(cls, refMap(defn.ObjectClass))
    else refMap(defn.ObjectClass)
  }

  override def prepareForValDef(vdef: ValDef)(using Context): Context =
    val sym = atPhase(thisPhase)(vdef.symbol)
    if captured.contains(sym) then
      val newd = atPhase(thisPhase)(sym.denot).copySymDenotation(
        info = refClass(sym.info.classSymbol, sym.hasAnnotation(defn.VolatileAnnot)).typeRef,
        initFlags = sym.flags &~ Mutable)
      newd.removeAnnotation(defn.VolatileAnnot)
      newd.installAfter(thisPhase)
    ctx

  override def transformValDef(vdef: ValDef)(using Context): Tree = {
    val vble = vdef.symbol
    if (captured.contains(vble)) {
      def boxMethod(name: TermName): Tree =
        ref(vble.info.classSymbol.companionModule.info.member(name).symbol)
      cpy.ValDef(vdef)(
        rhs = boxMethod(nme.create).appliedTo(vdef.rhs),
        tpt = TypeTree(vble.info).withSpan(vdef.tpt.span))
    }
    else vdef
  }

  override def transformIdent(id: Ident)(using Context): Tree = {
    val vble = id.symbol
    if (captured.contains(vble))
      id.select(nme.elem).ensureConforms(atPhase(thisPhase)(vble.denot).info)
    else id
  }

  /** If assignment is to a boxed ref type, e.g.
   *
   *      intRef.elem = expr
   *
   *  the lhs can be followed by a cast as an artifact of nested translation.
   *  In that case, drop the cast.
   *
   *  Then, when the LHS resolves to a method symbol — which happens when
   *  the boxing Ref class is Scala-defined (`final class IntRef(var elem:
   *  Int)`), so `elem` binds to the auto-generated getter rather than a
   *  Java-style public field — rewrite to a setter call. This mirrors
   *  `Getters.transformAssign`; that earlier phase only sees Assigns
   *  present before Erasure, while CapturedVars synthesises new Assigns
   *  afterwards that the same rule must apply to.
   */
  override def transformAssign(tree: Assign)(using Context): Tree =
    // Strip cast / 0-arg-Apply layers that Erasure may have wrapped
    // around the underlying Select. `becomes` only knows how to handle
    // an `Ident` or `Select` lhs. Apply recursively because casts can
    // nest over getter calls (`(qual.elem.asInstanceOf[T]).asInstanceOf[U]`)
    // or over post-Erasure Apply forms (`Apply(Select(_, elem), Nil)`).
    def unwrap(t: Tree): Tree = t match
      case TypeApply(Select(inner, nme.asInstanceOf_), _) => unwrap(inner)
      case Apply(sel: Select, Nil)                       => unwrap(sel)
      case other                                          => other
    val unwrapped = unwrap(tree.lhs)
    val lsym = unwrapped.symbol
    // Guard on `setter.exists` rather than just `is(Method)`: arbitrary
    // method-LHS shapes (e.g. cast-over-method, post-Erasure `Apply`
    // around a non-getter call) shouldn't be rewritten — only `var x`
    // getters whose paired setter actually exists. `becomes` would
    // otherwise assert when handed a non-setter, non-getter method
    // like `asInstanceOf`.
    if lsym.exists && lsym.is(Method) && lsym.setter.exists then
      unwrapped.becomes(tree.rhs).withSpan(tree.span)
    else if unwrapped eq tree.lhs then tree
    else cpy.Assign(tree)(unwrapped, tree.rhs)

object CapturedVars:
  val name: String = "capturedVars"
  val description: String = "represent vars captured by closures as heap objects"

  def collect(captured: util.HashSet[Symbol]): TreeTraverser = new:
    def traverse(tree: Tree)(using Context) = tree match
      case id: Ident =>
        val sym = id.symbol
        if sym.isMutableVar && sym.owner.isTerm then
          val enclMeth = ctx.owner.enclosingMethod
          if sym.enclosingMethod != enclMeth then
            report.log(i"capturing $sym in ${sym.enclosingMethod}, referenced from $enclMeth")
            captured += sym
      case _ =>
        traverseChildren(tree)
end CapturedVars
