package dotty.tools.backend.python

import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.compiletime.uninitialized

import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*
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

  @threadUnsafe lazy val NativeMethodRef = ScalaPythonPackageClass.requiredMethodRef("native")
  def NativeMethod(using Context): Symbol = NativeMethodRef.symbol

  /** Force all Phase 0 symbol lookups on a path that already runs today.
   *  This keeps the scaffolding exercised before Phase 1 starts using it. */
  def force()(using Context): Unit =
    val _ =
      ScalaPythonPackageVal
      ScalaPythonPackageClass
      PyAnyClass
      PyDynamicClass
      PyDynamic_selectDynamic
      PyDynamic_applyDynamic
      PyDynamic_applyDynamicNamed
      PyDynamic_updateDynamic
      DynamicModule
      DynamicModule_module
      DynamicModule_attr
      ExternAnnotClass
      NameAnnotClass
      NativeMethod
