package dotty.tools.backend.python

import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.compiletime.uninitialized

import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*
import SymDenotations.StubInfo
import Types.*

object PyDefinitions:
  private val cache = mutable.WeakHashMap.empty[Contexts.ContextBase, PyDefinitions]

  def pydefn(using Context): PyDefinitions =
    cache.synchronized {
      cache.getOrElseUpdate(ctx.base, {
        val defs = new PyDefinitions()
        defs.init()
        defs
      })
    }

final class PyDefinitions:

  private var initCtx: Context = uninitialized
  private given currentContext[Dummy_so_its_a_def]: Context = initCtx

  def init()(using Context): Unit =
    initCtx = ctx

  @threadUnsafe lazy val ScalaPythonPackageVal = requiredPackage("scala.python")
  @threadUnsafe lazy val ScalaPythonPackageClass = ScalaPythonPackageVal.moduleClass.asClass

  @threadUnsafe lazy val PyAnyClassType: TypeRef = requiredClassRef("scala.python.PyAny")
  def PyAnyClass(using Context): ClassSymbol = PyAnyClassType.symbol.asClass

  @threadUnsafe lazy val PyDynamicClassType: TypeRef = requiredClassRef("scala.python.PyDynamic")
  def PyDynamicClass(using Context): ClassSymbol = PyDynamicClassType.symbol.asClass
  @threadUnsafe lazy val PyDynamic_selectDynamicR = PyDynamicClass.requiredMethodRef("selectDynamic")
  def PyDynamic_selectDynamic(using Context): Symbol = PyDynamic_selectDynamicR.symbol
  @threadUnsafe lazy val PyDynamic_applyDynamicR = PyDynamicClass.requiredMethodRef("applyDynamic")
  def PyDynamic_applyDynamic(using Context): Symbol = PyDynamic_applyDynamicR.symbol
  @threadUnsafe lazy val PyDynamic_applyDynamicNamedR = PyDynamicClass.requiredMethodRef("applyDynamicNamed")
  def PyDynamic_applyDynamicNamed(using Context): Symbol = PyDynamic_applyDynamicNamedR.symbol
  @threadUnsafe lazy val PyDynamic_updateDynamicR = PyDynamicClass.requiredMethodRef("updateDynamic")
  def PyDynamic_updateDynamic(using Context): Symbol = PyDynamic_updateDynamicR.symbol

  @threadUnsafe lazy val DynamicModuleRef = requiredModuleRef("scala.python.Dynamic")
  def DynamicModule(using Context): Symbol = DynamicModuleRef.symbol
  @threadUnsafe lazy val DynamicModule_moduleR = DynamicModule.requiredMethodRef("module")
  def DynamicModule_module(using Context): Symbol = DynamicModule_moduleR.symbol
  @threadUnsafe lazy val DynamicModule_attrR = DynamicModule.requiredMethodRef("attr")
  def DynamicModule_attr(using Context): Symbol = DynamicModule_attrR.symbol

  @threadUnsafe lazy val ExternAnnotType: TypeRef = requiredClassRef("scala.python.extern")
  def ExternAnnotClass(using Context): ClassSymbol = ExternAnnotType.symbol.asClass

  @threadUnsafe lazy val NameAnnotType: TypeRef = requiredClassRef("scala.python.name")
  def NameAnnotClass(using Context): ClassSymbol = NameAnnotType.symbol.asClass

  // `scala.python.native` is defined as `inline def`: every call site is
  // substituted at typer time and no method symbol is ever referenced at
  // codegen. We intentionally do NOT cache a NativeMethod symbol here.

  // ---------------------------------------------------------------
  //  Tuple intrinsics
  //
  //  When `-scalapy` is on, the backend lowers Scala tuple values to
  //  native Python tuples. The symbols below identify the call sites
  //  that get rewritten in `GenPython.genTupleCallOpt`.
  // ---------------------------------------------------------------

  @threadUnsafe lazy val TupleClassType: TypeRef = requiredClassRef("scala.Tuple")
  def TupleClass(using Context): ClassSymbol = TupleClassType.symbol.asClass

  @threadUnsafe lazy val NonEmptyTupleClassType: TypeRef = requiredClassRef("scala.NonEmptyTuple")
  def NonEmptyTupleClass(using Context): ClassSymbol = NonEmptyTupleClassType.symbol.asClass

  @threadUnsafe lazy val PairClassType: TypeRef = requiredClassRef("scala.*:")
  def PairClass(using Context): ClassSymbol = PairClassType.symbol.asClass

  @threadUnsafe lazy val EmptyTupleModuleRef = requiredModuleRef("scala.EmptyTuple")
  def EmptyTupleModule(using Context): Symbol = EmptyTupleModuleRef.symbol
  def EmptyTupleClass(using Context): ClassSymbol = EmptyTupleModule.moduleClass.asClass

  @threadUnsafe lazy val TupleXXLClassType: TypeRef = requiredClassRef("scala.runtime.TupleXXL")
  def TupleXXLClass(using Context): ClassSymbol = TupleXXLClassType.symbol.asClass

  /** `scala.Tuple1` … `scala.Tuple22`. Indexed by arity (1..22). */
  @threadUnsafe lazy val TupleNClassTypes: Array[TypeRef] =
    val arr = new Array[TypeRef](23)
    for n <- 1 to 22 do arr(n) = requiredClassRef(s"scala.Tuple$n")
    arr
  def TupleNClass(arity: Int)(using Context): ClassSymbol =
    TupleNClassTypes(arity).symbol.asClass

  @threadUnsafe lazy val Tuple2_swapR =
    TupleNClassTypes(2).symbol.asClass.requiredMethodRef("swap")
  def Tuple2_swap(using Context): Symbol = Tuple2_swapR.symbol

  // `scala.Product` polymorphic methods. Intercepted unconditionally so
  // they work on Python tuples (which have no encoded Product methods)
  // and case classes (which do) via a runtime branch. Erasure can hide
  // a tuple value under a `Product`/`Any` static type, so a strict
  // static-receiver-type check would miss those call sites.
  def ProductClassSym(using Context): ClassSymbol = defn.ProductClass
  @threadUnsafe lazy val Product_productArityR        = defn.ProductClass.requiredMethodRef("productArity")
  @threadUnsafe lazy val Product_productElementR      = defn.ProductClass.requiredMethodRef("productElement")
  @threadUnsafe lazy val Product_productIteratorR     = defn.ProductClass.requiredMethodRef("productIterator")
  @threadUnsafe lazy val Product_productPrefixR       = defn.ProductClass.requiredMethodRef("productPrefix")
  @threadUnsafe lazy val Product_productElementNameR  = defn.ProductClass.requiredMethodRef("productElementName")
  def Product_productArity(using Context): Symbol       = Product_productArityR.symbol
  def Product_productElement(using Context): Symbol     = Product_productElementR.symbol
  def Product_productIterator(using Context): Symbol    = Product_productIteratorR.symbol
  def Product_productPrefix(using Context): Symbol      = Product_productPrefixR.symbol
  def Product_productElementName(using Context): Symbol = Product_productElementNameR.symbol

  @threadUnsafe lazy val RuntimeTuplesModuleRef = requiredModuleRef("scala.runtime.Tuples")
  def RuntimeTuplesModule(using Context): Symbol = RuntimeTuplesModuleRef.symbol
  private def runtimeTuplesMethodRef(name: String)(using Context): TermRef =
    RuntimeTuplesModule.requiredMethodRef(name)

  @threadUnsafe lazy val Tuples_applyR        = runtimeTuplesMethodRef("apply")
  @threadUnsafe lazy val Tuples_consR         = runtimeTuplesMethodRef("cons")
  @threadUnsafe lazy val Tuples_appendR       = runtimeTuplesMethodRef("append")
  @threadUnsafe lazy val Tuples_concatR       = runtimeTuplesMethodRef("concat")
  @threadUnsafe lazy val Tuples_tailR         = runtimeTuplesMethodRef("tail")
  @threadUnsafe lazy val Tuples_initR         = runtimeTuplesMethodRef("init")
  @threadUnsafe lazy val Tuples_lastR         = runtimeTuplesMethodRef("last")
  @threadUnsafe lazy val Tuples_sizeR         = runtimeTuplesMethodRef("size")
  @threadUnsafe lazy val Tuples_reverseR      = runtimeTuplesMethodRef("reverse")
  @threadUnsafe lazy val Tuples_takeR         = runtimeTuplesMethodRef("take")
  @threadUnsafe lazy val Tuples_dropR         = runtimeTuplesMethodRef("drop")
  @threadUnsafe lazy val Tuples_splitAtR      = runtimeTuplesMethodRef("splitAt")
  @threadUnsafe lazy val Tuples_zipR          = runtimeTuplesMethodRef("zip")
  @threadUnsafe lazy val Tuples_mapR          = runtimeTuplesMethodRef("map")
  @threadUnsafe lazy val Tuples_toArrayR      = runtimeTuplesMethodRef("toArray")
  @threadUnsafe lazy val Tuples_toIArrayR     = runtimeTuplesMethodRef("toIArray")
  @threadUnsafe lazy val Tuples_fromArrayR    = runtimeTuplesMethodRef("fromArray")
  @threadUnsafe lazy val Tuples_fromIArrayR   = runtimeTuplesMethodRef("fromIArray")
  @threadUnsafe lazy val Tuples_fromProductR  = runtimeTuplesMethodRef("fromProduct")
  @threadUnsafe lazy val Tuples_isInstanceOfTupleR         = runtimeTuplesMethodRef("isInstanceOfTuple")
  @threadUnsafe lazy val Tuples_isInstanceOfEmptyTupleR    = runtimeTuplesMethodRef("isInstanceOfEmptyTuple")
  @threadUnsafe lazy val Tuples_isInstanceOfNonEmptyTupleR = runtimeTuplesMethodRef("isInstanceOfNonEmptyTuple")

  def Tuples_apply(using Context): Symbol        = Tuples_applyR.symbol
  def Tuples_cons(using Context): Symbol         = Tuples_consR.symbol
  def Tuples_append(using Context): Symbol       = Tuples_appendR.symbol
  def Tuples_concat(using Context): Symbol       = Tuples_concatR.symbol
  def Tuples_tail(using Context): Symbol         = Tuples_tailR.symbol
  def Tuples_init(using Context): Symbol         = Tuples_initR.symbol
  def Tuples_last(using Context): Symbol         = Tuples_lastR.symbol
  def Tuples_size(using Context): Symbol         = Tuples_sizeR.symbol
  def Tuples_reverse(using Context): Symbol      = Tuples_reverseR.symbol
  def Tuples_take(using Context): Symbol         = Tuples_takeR.symbol
  def Tuples_drop(using Context): Symbol         = Tuples_dropR.symbol
  def Tuples_splitAt(using Context): Symbol      = Tuples_splitAtR.symbol
  def Tuples_zip(using Context): Symbol          = Tuples_zipR.symbol
  def Tuples_map(using Context): Symbol          = Tuples_mapR.symbol
  def Tuples_toArray(using Context): Symbol      = Tuples_toArrayR.symbol
  def Tuples_toIArray(using Context): Symbol     = Tuples_toIArrayR.symbol
  def Tuples_fromArray(using Context): Symbol    = Tuples_fromArrayR.symbol
  def Tuples_fromIArray(using Context): Symbol   = Tuples_fromIArrayR.symbol
  def Tuples_fromProduct(using Context): Symbol  = Tuples_fromProductR.symbol
  def Tuples_isInstanceOfTuple(using Context): Symbol         = Tuples_isInstanceOfTupleR.symbol
  def Tuples_isInstanceOfEmptyTuple(using Context): Symbol    = Tuples_isInstanceOfEmptyTupleR.symbol
  def Tuples_isInstanceOfNonEmptyTuple(using Context): Symbol = Tuples_isInstanceOfNonEmptyTupleR.symbol

  /** True iff `sym` is the class of a tuple instance.
   *
   *  Scala 3's `Tuple1..22` only extend `Product{N}` — they do NOT
   *  syntactically extend the `Tuple` trait — so a plain
   *  `derivesFrom(TupleClass)` check misses them. We mirror dotc's
   *  `defn.isTupleClass` (name-based: `Tuple1..22`) and add the cons
   *  class `*:`, the `Tuple`/`NonEmptyTuple` traits, the `EmptyTuple`
   *  case object's class, `TupleXXL`, and anything that derives from
   *  the `*:` cons class (which catches specialized forms such as
   *  `Tuple2$mcII$sp`). */
  def isTupleClass(sym: Symbol)(using Context): Boolean =
    if !sym.exists || !sym.isClass then return false
    if defn.isTupleClass(sym) then return true
    val cs = sym.asClass
    cs == TupleClass
      || cs == NonEmptyTupleClass
      || cs == EmptyTupleClass
      || cs == PairClass
      || cs == TupleXXLClass
      || cs.derivesFrom(PairClass)

  /** Force every Phase 0 symbol lookup and assert each one resolves to a
   *  real classpath entry rather than a silently generated stub.
   *
   *  `requiredClass` / `requiredPackage` / `requiredMethod` return stub
   *  symbols for missing references by default (see
   *  `Denotations.requiredSymbol` + `Symbols.newStubSymbol`), so the mere
   *  fact that these accessors return a non-null `Symbol` is not evidence
   *  the library is on the classpath. We detect stubs by peeking at
   *  `infoOrCompleter` — for missing references it is a `StubInfo`.
   *
   *  If any symbol is a stub we throw loudly. `scala.python.*` lives only
   *  in the `scala-library-py` sidecar jar — if the user classpath omits
   *  it while compiling with `-scalapy` we want a clear error, not a stub
   *  symbol silently threaded into later phases. */
  def force()(using Context): Unit =
    requireReal(ScalaPythonPackageVal, "scala.python")
    requireReal(ScalaPythonPackageClass, "scala.python (package class)")
    requireReal(PyAnyClass, "scala.python.PyAny")
    requireReal(PyDynamicClass, "scala.python.PyDynamic")
    requireReal(PyDynamic_selectDynamic, "scala.python.PyDynamic.selectDynamic")
    requireReal(PyDynamic_applyDynamic, "scala.python.PyDynamic.applyDynamic")
    requireReal(PyDynamic_applyDynamicNamed, "scala.python.PyDynamic.applyDynamicNamed")
    requireReal(PyDynamic_updateDynamic, "scala.python.PyDynamic.updateDynamic")
    requireReal(DynamicModule, "scala.python.Dynamic")
    requireReal(DynamicModule_module, "scala.python.Dynamic.module")
    requireReal(DynamicModule_attr, "scala.python.Dynamic.attr")
    requireReal(ExternAnnotClass, "scala.python.extern")
    requireReal(NameAnnotClass, "scala.python.name")
    requireReal(TupleClass, "scala.Tuple")
    requireReal(NonEmptyTupleClass, "scala.NonEmptyTuple")
    requireReal(PairClass, "scala.*:")
    requireReal(EmptyTupleModule, "scala.EmptyTuple")
    requireReal(TupleXXLClass, "scala.runtime.TupleXXL")
    for n <- 1 to 22 do requireReal(TupleNClass(n), s"scala.Tuple$n")
    requireReal(Tuple2_swap, "scala.Tuple2.swap")
    requireReal(RuntimeTuplesModule, "scala.runtime.Tuples")
    requireReal(Tuples_apply, "scala.runtime.Tuples.apply")
    requireReal(Tuples_cons, "scala.runtime.Tuples.cons")
    requireReal(Tuples_append, "scala.runtime.Tuples.append")
    requireReal(Tuples_concat, "scala.runtime.Tuples.concat")
    requireReal(Tuples_tail, "scala.runtime.Tuples.tail")
    requireReal(Tuples_init, "scala.runtime.Tuples.init")
    requireReal(Tuples_last, "scala.runtime.Tuples.last")
    requireReal(Tuples_size, "scala.runtime.Tuples.size")
    requireReal(Tuples_reverse, "scala.runtime.Tuples.reverse")
    requireReal(Tuples_take, "scala.runtime.Tuples.take")
    requireReal(Tuples_drop, "scala.runtime.Tuples.drop")
    requireReal(Tuples_splitAt, "scala.runtime.Tuples.splitAt")
    requireReal(Tuples_zip, "scala.runtime.Tuples.zip")
    requireReal(Tuples_map, "scala.runtime.Tuples.map")
    requireReal(Tuples_toArray, "scala.runtime.Tuples.toArray")
    requireReal(Tuples_toIArray, "scala.runtime.Tuples.toIArray")
    requireReal(Tuples_fromArray, "scala.runtime.Tuples.fromArray")
    requireReal(Tuples_fromIArray, "scala.runtime.Tuples.fromIArray")
    requireReal(Tuples_fromProduct, "scala.runtime.Tuples.fromProduct")
    requireReal(Tuples_isInstanceOfTuple, "scala.runtime.Tuples.isInstanceOfTuple")
    requireReal(Tuples_isInstanceOfEmptyTuple, "scala.runtime.Tuples.isInstanceOfEmptyTuple")
    requireReal(Tuples_isInstanceOfNonEmptyTuple, "scala.runtime.Tuples.isInstanceOfNonEmptyTuple")

  private def requireReal(sym: Symbol, desc: String)(using Context): Unit =
    if !sym.exists then
      throw AssertionError(s"PyDefinitions: $desc not found on classpath")
    sym.denot.infoOrCompleter match
      case _: StubInfo =>
        throw AssertionError(
          s"PyDefinitions: $desc resolved to a stub symbol; " +
            s"`scala.python.*` appears to be missing from the compiler classpath")
      case _ => ()
