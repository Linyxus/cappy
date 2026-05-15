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

  /** Drop the cached `PyDefinitions` for a given `ContextBase`. Used by
   *  the Cappy REPL between runs: each REPL input is its own `Run`, but
   *  the `ContextBase` is shared across runs so the cached lazy vals
   *  capture symbol denotations from the *first* run. Those denotations
   *  expire on the next run boundary ("denotation class X invalid in
   *  run N"), so the next force lookups fail. Invalidating between runs
   *  keeps the cache honest. */
  def invalidate(base: Contexts.ContextBase): Unit =
    cache.synchronized {
      cache.remove(base)
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

  // ---------------------------------------------------------------
  //  PyMap intrinsics
  //
  //  When `-scalapy` is on, the backend lowers `scala.python.PyMap`
  //  values to native Python dicts. The symbols below identify the
  //  call sites that get rewritten in `GenPython.genPyMapCallOpt`.
  // ---------------------------------------------------------------

  @threadUnsafe lazy val PyMapClassType: TypeRef = requiredClassRef("scala.python.PyMap")
  def PyMapClass(using Context): ClassSymbol = PyMapClassType.symbol.asClass

  @threadUnsafe lazy val PyMapModuleRef = requiredModuleRef("scala.python.PyMap")
  def PyMapModule(using Context): Symbol = PyMapModuleRef.symbol

  private def pyMapClassMethod(name: String)(using Context): TermRef =
    PyMapClass.requiredMethodRef(name)
  private def pyMapModuleMethod(name: String)(using Context): TermRef =
    PyMapModule.requiredMethodRef(name)

  @threadUnsafe lazy val PyMap_emptyR        = pyMapModuleMethod("empty")
  @threadUnsafe lazy val PyMap_sizeR         = pyMapClassMethod("size")
  @threadUnsafe lazy val PyMap_isEmptyR      = pyMapClassMethod("isEmpty")
  @threadUnsafe lazy val PyMap_nonEmptyR     = pyMapClassMethod("nonEmpty")
  @threadUnsafe lazy val PyMap_applyR        = pyMapClassMethod("apply")
  @threadUnsafe lazy val PyMap_getR          = pyMapClassMethod("get")
  @threadUnsafe lazy val PyMap_getOrElseR    = pyMapClassMethod("getOrElse")
  @threadUnsafe lazy val PyMap_updateR       = pyMapClassMethod("update")
  @threadUnsafe lazy val PyMap_deleteR       = pyMapClassMethod("delete")
  @threadUnsafe lazy val PyMap_setDefaultR   = pyMapClassMethod("setDefault")
  @threadUnsafe lazy val PyMap_popR          = pyMapClassMethod("pop")
  @threadUnsafe lazy val PyMap_popOrElseR    = pyMapClassMethod("popOrElse")
  @threadUnsafe lazy val PyMap_popItemR      = pyMapClassMethod("popItem")
  @threadUnsafe lazy val PyMap_clearR        = pyMapClassMethod("clear")
  @threadUnsafe lazy val PyMap_updateAllR    = pyMapClassMethod("updateAll")
  @threadUnsafe lazy val PyMap_containsR     = pyMapClassMethod("contains")
  @threadUnsafe lazy val PyMap_copyR         = pyMapClassMethod("copy")
  @threadUnsafe lazy val PyMap_mergedR       = pyMapClassMethod("merged")
  @threadUnsafe lazy val PyMap_mergeInPlaceR = pyMapClassMethod("mergeInPlace")
  @threadUnsafe lazy val PyMap_keysR         = pyMapClassMethod("keys")
  @threadUnsafe lazy val PyMap_valuesR       = pyMapClassMethod("values")
  @threadUnsafe lazy val PyMap_itemsR        = pyMapClassMethod("items")

  def PyMap_empty(using Context): Symbol        = PyMap_emptyR.symbol
  def PyMap_size(using Context): Symbol         = PyMap_sizeR.symbol
  def PyMap_isEmpty(using Context): Symbol      = PyMap_isEmptyR.symbol
  def PyMap_nonEmpty(using Context): Symbol     = PyMap_nonEmptyR.symbol
  def PyMap_apply(using Context): Symbol        = PyMap_applyR.symbol
  def PyMap_get(using Context): Symbol          = PyMap_getR.symbol
  def PyMap_getOrElse(using Context): Symbol    = PyMap_getOrElseR.symbol
  def PyMap_update(using Context): Symbol       = PyMap_updateR.symbol
  def PyMap_delete(using Context): Symbol       = PyMap_deleteR.symbol
  def PyMap_setDefault(using Context): Symbol   = PyMap_setDefaultR.symbol
  def PyMap_pop(using Context): Symbol          = PyMap_popR.symbol
  def PyMap_popOrElse(using Context): Symbol    = PyMap_popOrElseR.symbol
  def PyMap_popItem(using Context): Symbol      = PyMap_popItemR.symbol
  def PyMap_clear(using Context): Symbol        = PyMap_clearR.symbol
  def PyMap_updateAll(using Context): Symbol    = PyMap_updateAllR.symbol
  def PyMap_contains(using Context): Symbol     = PyMap_containsR.symbol
  def PyMap_copy(using Context): Symbol         = PyMap_copyR.symbol
  def PyMap_merged(using Context): Symbol       = PyMap_mergedR.symbol
  def PyMap_mergeInPlace(using Context): Symbol = PyMap_mergeInPlaceR.symbol
  def PyMap_keys(using Context): Symbol         = PyMap_keysR.symbol
  def PyMap_values(using Context): Symbol       = PyMap_valuesR.symbol
  def PyMap_items(using Context): Symbol        = PyMap_itemsR.symbol

  // ---------------------------------------------------------------
  //  PyList intrinsics
  //
  //  When `-scalapy` is on, the backend lowers `scala.python.PyList`
  //  values to native Python lists. The symbols below identify the
  //  call sites that get rewritten in `GenPython.genPyListCallOpt`.
  // ---------------------------------------------------------------

  @threadUnsafe lazy val PyListClassType: TypeRef = requiredClassRef("scala.python.PyList")
  def PyListClass(using Context): ClassSymbol = PyListClassType.symbol.asClass

  @threadUnsafe lazy val PyListModuleRef = requiredModuleRef("scala.python.PyList")
  def PyListModule(using Context): Symbol = PyListModuleRef.symbol

  private def pyListClassMethod(name: String)(using Context): TermRef =
    PyListClass.requiredMethodRef(name)
  private def pyListModuleMethod(name: String)(using Context): TermRef =
    PyListModule.requiredMethodRef(name)

  @threadUnsafe lazy val PyList_emptyR        = pyListModuleMethod("empty")
  @threadUnsafe lazy val PyList_applyFactoryR = pyListModuleMethod("apply")
  @threadUnsafe lazy val PyList_sizeR         = pyListClassMethod("size")
  @threadUnsafe lazy val PyList_isEmptyR      = pyListClassMethod("isEmpty")
  @threadUnsafe lazy val PyList_nonEmptyR     = pyListClassMethod("nonEmpty")
  @threadUnsafe lazy val PyList_applyR        = pyListClassMethod("apply")
  @threadUnsafe lazy val PyList_updateR       = pyListClassMethod("update")
  @threadUnsafe lazy val PyList_appendR       = pyListClassMethod("append")
  @threadUnsafe lazy val PyList_prependR      = pyListClassMethod("prepend")
  @threadUnsafe lazy val PyList_insertR       = pyListClassMethod("insert")
  @threadUnsafe lazy val PyList_removeAtR     = pyListClassMethod("removeAt")
  @threadUnsafe lazy val PyList_removeR       = pyListClassMethod("remove")
  @threadUnsafe lazy val PyList_clearR        = pyListClassMethod("clear")
  @threadUnsafe lazy val PyList_extendR       = pyListClassMethod("extend")
  @threadUnsafe lazy val PyList_containsR     = pyListClassMethod("contains")
  @threadUnsafe lazy val PyList_indexOfR      = pyListClassMethod("indexOf")
  @threadUnsafe lazy val PyList_countR        = pyListClassMethod("count")
  @threadUnsafe lazy val PyList_copyR         = pyListClassMethod("copy")
  @threadUnsafe lazy val PyList_concatR       = pyListClassMethod("concat")
  @threadUnsafe lazy val PyList_sliceR        = pyListClassMethod("slice")
  @threadUnsafe lazy val PyList_sortR         = pyListClassMethod("sort")
  @threadUnsafe lazy val PyList_reverseR      = pyListClassMethod("reverse")
  @threadUnsafe lazy val PyList_iteratorR     = pyListClassMethod("iterator")

  def PyList_empty(using Context): Symbol        = PyList_emptyR.symbol
  def PyList_applyFactory(using Context): Symbol = PyList_applyFactoryR.symbol
  def PyList_size(using Context): Symbol         = PyList_sizeR.symbol
  def PyList_isEmpty(using Context): Symbol      = PyList_isEmptyR.symbol
  def PyList_nonEmpty(using Context): Symbol     = PyList_nonEmptyR.symbol
  def PyList_apply(using Context): Symbol        = PyList_applyR.symbol
  def PyList_update(using Context): Symbol       = PyList_updateR.symbol
  def PyList_append(using Context): Symbol       = PyList_appendR.symbol
  def PyList_prepend(using Context): Symbol      = PyList_prependR.symbol
  def PyList_insert(using Context): Symbol       = PyList_insertR.symbol
  def PyList_removeAt(using Context): Symbol     = PyList_removeAtR.symbol
  def PyList_remove(using Context): Symbol       = PyList_removeR.symbol
  def PyList_clear(using Context): Symbol        = PyList_clearR.symbol
  def PyList_extend(using Context): Symbol       = PyList_extendR.symbol
  def PyList_contains(using Context): Symbol     = PyList_containsR.symbol
  def PyList_indexOf(using Context): Symbol      = PyList_indexOfR.symbol
  def PyList_count(using Context): Symbol        = PyList_countR.symbol
  def PyList_copy(using Context): Symbol         = PyList_copyR.symbol
  def PyList_concat(using Context): Symbol       = PyList_concatR.symbol
  def PyList_slice(using Context): Symbol        = PyList_sliceR.symbol
  def PyList_sort(using Context): Symbol         = PyList_sortR.symbol
  def PyList_reverse(using Context): Symbol      = PyList_reverseR.symbol
  def PyList_iterator(using Context): Symbol     = PyList_iteratorR.symbol

  // ---------------------------------------------------------------
  //  PyTuple intrinsics
  //
  //  When `-scalapy` is on, the backend lowers `scala.python.PyTuple`
  //  values to native (bare) Python tuples. The symbols below identify
  //  the call sites that get rewritten in `GenPython.genPyTupleCallOpt`.
  //  Distinct from Scala `TupleN` lowering (which wraps in
  //  `_scpy_ScalaTuple`).
  // ---------------------------------------------------------------

  @threadUnsafe lazy val PyTupleClassType: TypeRef = requiredClassRef("scala.python.PyTuple")
  def PyTupleClass(using Context): ClassSymbol = PyTupleClassType.symbol.asClass

  @threadUnsafe lazy val PyTupleModuleRef = requiredModuleRef("scala.python.PyTuple")
  def PyTupleModule(using Context): Symbol = PyTupleModuleRef.symbol

  private def pyTupleClassMethod(name: String)(using Context): TermRef =
    PyTupleClass.requiredMethodRef(name)
  private def pyTupleModuleMethod(name: String)(using Context): TermRef =
    PyTupleModule.requiredMethodRef(name)

  @threadUnsafe lazy val PyTuple_emptyR        = pyTupleModuleMethod("empty")
  @threadUnsafe lazy val PyTuple_applyFactoryR = pyTupleModuleMethod("apply")
  @threadUnsafe lazy val PyTuple_sizeR         = pyTupleClassMethod("size")
  @threadUnsafe lazy val PyTuple_isEmptyR      = pyTupleClassMethod("isEmpty")
  @threadUnsafe lazy val PyTuple_nonEmptyR     = pyTupleClassMethod("nonEmpty")
  @threadUnsafe lazy val PyTuple_applyR        = pyTupleClassMethod("apply")
  @threadUnsafe lazy val PyTuple_containsR     = pyTupleClassMethod("contains")
  @threadUnsafe lazy val PyTuple_indexOfR      = pyTupleClassMethod("indexOf")
  @threadUnsafe lazy val PyTuple_countR        = pyTupleClassMethod("count")
  @threadUnsafe lazy val PyTuple_concatR       = pyTupleClassMethod("concat")
  @threadUnsafe lazy val PyTuple_sliceR        = pyTupleClassMethod("slice")
  @threadUnsafe lazy val PyTuple_iteratorR     = pyTupleClassMethod("iterator")

  def PyTuple_empty(using Context): Symbol        = PyTuple_emptyR.symbol
  def PyTuple_applyFactory(using Context): Symbol = PyTuple_applyFactoryR.symbol
  def PyTuple_size(using Context): Symbol         = PyTuple_sizeR.symbol
  def PyTuple_isEmpty(using Context): Symbol      = PyTuple_isEmptyR.symbol
  def PyTuple_nonEmpty(using Context): Symbol     = PyTuple_nonEmptyR.symbol
  def PyTuple_apply(using Context): Symbol        = PyTuple_applyR.symbol
  def PyTuple_contains(using Context): Symbol     = PyTuple_containsR.symbol
  def PyTuple_indexOf(using Context): Symbol      = PyTuple_indexOfR.symbol
  def PyTuple_count(using Context): Symbol        = PyTuple_countR.symbol
  def PyTuple_concat(using Context): Symbol       = PyTuple_concatR.symbol
  def PyTuple_slice(using Context): Symbol        = PyTuple_sliceR.symbol
  def PyTuple_iterator(using Context): Symbol     = PyTuple_iteratorR.symbol

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
    requireReal(PyMapClass, "scala.python.PyMap")
    requireReal(PyMapModule, "scala.python.PyMap (module)")
    requireReal(PyMap_empty, "scala.python.PyMap.empty")
    requireReal(PyMap_size, "scala.python.PyMap.size")
    requireReal(PyMap_isEmpty, "scala.python.PyMap.isEmpty")
    requireReal(PyMap_nonEmpty, "scala.python.PyMap.nonEmpty")
    requireReal(PyMap_apply, "scala.python.PyMap.apply")
    requireReal(PyMap_get, "scala.python.PyMap.get")
    requireReal(PyMap_getOrElse, "scala.python.PyMap.getOrElse")
    requireReal(PyMap_update, "scala.python.PyMap.update")
    requireReal(PyMap_delete, "scala.python.PyMap.delete")
    requireReal(PyMap_setDefault, "scala.python.PyMap.setDefault")
    requireReal(PyMap_pop, "scala.python.PyMap.pop")
    requireReal(PyMap_popOrElse, "scala.python.PyMap.popOrElse")
    requireReal(PyMap_popItem, "scala.python.PyMap.popItem")
    requireReal(PyMap_clear, "scala.python.PyMap.clear")
    requireReal(PyMap_updateAll, "scala.python.PyMap.updateAll")
    requireReal(PyMap_contains, "scala.python.PyMap.contains")
    requireReal(PyMap_copy, "scala.python.PyMap.copy")
    requireReal(PyMap_merged, "scala.python.PyMap.merged")
    requireReal(PyMap_mergeInPlace, "scala.python.PyMap.mergeInPlace")
    requireReal(PyMap_keys, "scala.python.PyMap.keys")
    requireReal(PyMap_values, "scala.python.PyMap.values")
    requireReal(PyMap_items, "scala.python.PyMap.items")
    requireReal(PyListClass, "scala.python.PyList")
    requireReal(PyListModule, "scala.python.PyList (module)")
    requireReal(PyList_empty, "scala.python.PyList.empty")
    requireReal(PyList_applyFactory, "scala.python.PyList.apply (module)")
    requireReal(PyList_size, "scala.python.PyList.size")
    requireReal(PyList_isEmpty, "scala.python.PyList.isEmpty")
    requireReal(PyList_nonEmpty, "scala.python.PyList.nonEmpty")
    requireReal(PyList_apply, "scala.python.PyList.apply")
    requireReal(PyList_update, "scala.python.PyList.update")
    requireReal(PyList_append, "scala.python.PyList.append")
    requireReal(PyList_prepend, "scala.python.PyList.prepend")
    requireReal(PyList_insert, "scala.python.PyList.insert")
    requireReal(PyList_removeAt, "scala.python.PyList.removeAt")
    requireReal(PyList_remove, "scala.python.PyList.remove")
    requireReal(PyList_clear, "scala.python.PyList.clear")
    requireReal(PyList_extend, "scala.python.PyList.extend")
    requireReal(PyList_contains, "scala.python.PyList.contains")
    requireReal(PyList_indexOf, "scala.python.PyList.indexOf")
    requireReal(PyList_count, "scala.python.PyList.count")
    requireReal(PyList_copy, "scala.python.PyList.copy")
    requireReal(PyList_concat, "scala.python.PyList.concat")
    requireReal(PyList_slice, "scala.python.PyList.slice")
    requireReal(PyList_sort, "scala.python.PyList.sort")
    requireReal(PyList_reverse, "scala.python.PyList.reverse")
    requireReal(PyList_iterator, "scala.python.PyList.iterator")
    requireReal(PyTupleClass, "scala.python.PyTuple")
    requireReal(PyTupleModule, "scala.python.PyTuple (module)")
    requireReal(PyTuple_empty, "scala.python.PyTuple.empty")
    requireReal(PyTuple_applyFactory, "scala.python.PyTuple.apply (module)")
    requireReal(PyTuple_size, "scala.python.PyTuple.size")
    requireReal(PyTuple_isEmpty, "scala.python.PyTuple.isEmpty")
    requireReal(PyTuple_nonEmpty, "scala.python.PyTuple.nonEmpty")
    requireReal(PyTuple_apply, "scala.python.PyTuple.apply")
    requireReal(PyTuple_contains, "scala.python.PyTuple.contains")
    requireReal(PyTuple_indexOf, "scala.python.PyTuple.indexOf")
    requireReal(PyTuple_count, "scala.python.PyTuple.count")
    requireReal(PyTuple_concat, "scala.python.PyTuple.concat")
    requireReal(PyTuple_slice, "scala.python.PyTuple.slice")
    requireReal(PyTuple_iterator, "scala.python.PyTuple.iterator")

  private def requireReal(sym: Symbol, desc: String)(using Context): Unit =
    if !sym.exists then
      throw AssertionError(s"PyDefinitions: $desc not found on classpath")
    sym.denot.infoOrCompleter match
      case _: StubInfo =>
        throw AssertionError(
          s"PyDefinitions: $desc resolved to a stub symbol; " +
            s"`scala.python.*` appears to be missing from the compiler classpath")
      case _ => ()
