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

  private def requireReal(sym: Symbol, desc: String)(using Context): Unit =
    if !sym.exists then
      throw AssertionError(s"PyDefinitions: $desc not found on classpath")
    sym.denot.infoOrCompleter match
      case _: StubInfo =>
        throw AssertionError(
          s"PyDefinitions: $desc resolved to a stub symbol; " +
            s"`scala.python.*` appears to be missing from the compiler classpath")
      case _ => ()
