package cavia
package codegen

import core.*
import ast.{Expr, Wasm}
import cavia.core.ast.Expr.Term
import Wasm.*
import typechecking.*
import scala.collection.mutable
import mutable.ArrayBuffer
import cavia.core.ast.Expr.PrimitiveOp
import cavia.core.ast.Expr.Definition
import java.util.IdentityHashMap
import scala.jdk.CollectionConverters._

object CodeGenerator:
  case class ClosureTypeInfo(funcTypeSym: Symbol, closTypeSym: Symbol)

  enum BinderInfo:
    case Sym(binder: Expr.Binder, sym: Symbol)
    case Inaccessible(binder: Expr.Binder)

    val binder: Expr.Binder

  enum DefInfo:
    case FuncDef(funcSym: Symbol, workerSym: Symbol)
    case GlobalDef(globalSym: Symbol)

  case class Context(
    funcs: ArrayBuffer[Func] = ArrayBuffer.empty,
    globals: ArrayBuffer[Global] = ArrayBuffer.empty,
    exports: ArrayBuffer[Export] = ArrayBuffer.empty,
    imports: ArrayBuffer[ImportFunc] = ArrayBuffer.empty,
    locals: ArrayBuffer[(Symbol, ValType)] = ArrayBuffer.empty,
    types: ArrayBuffer[TypeDef] = ArrayBuffer.empty,
    closureTypes: mutable.Map[FuncType, ClosureTypeInfo] = mutable.Map.empty,
    declares: ArrayBuffer[ElemDeclare] = ArrayBuffer.empty,
    binderInfos: List[BinderInfo] = Nil,
    defInfos: mutable.Map[Expr.Symbol, DefInfo] = new IdentityHashMap[Expr.Symbol, DefInfo]().asScala,
    var startFunc: Option[Symbol] = None,
  ):
    def withLocalSym(binder: Expr.Binder, sym: Symbol): Context =
      copy(binderInfos = BinderInfo.Sym(binder, sym) :: binderInfos)
    
    def usingBinderInfos(binderInfos: List[BinderInfo]): Context =
      copy(binderInfos = binderInfos)

  def ctx(using ctx: Context): Context = ctx

  def emitFunc(f: Func)(using Context): Unit =
    ctx.funcs += f

  def emitExport(e: Export)(using Context): Unit =
    ctx.exports += e

  def emitGlobal(g: Global)(using Context): Unit =
    ctx.globals += g

  def emitImportFunc(i: ImportFunc)(using Context): Unit =
    ctx.imports += i

  def emitLocal(sym: Symbol, tpe: ValType)(using Context): Unit =
    ctx.locals += (sym -> tpe)

  def emitLocals(locals: List[(Symbol, ValType)])(using Context): Unit =
    locals.foreach: (sym, tpe) =>
      emitLocal(sym, tpe)

  def emitType(t: TypeDef)(using Context): Unit =
    ctx.types += t

  def emitElemDeclare(kind: ExportKind, sym: Symbol)(using Context): Unit =
    ctx.declares += ElemDeclare(kind, sym)

  def finalizeLocals(using Context): List[(Symbol, ValType)] =
    val result = ctx.locals.toList
    ctx.locals.clear()
    result

  def translatePrimOp(op: PrimitiveOp)(using Context): List[Instruction] = op match
    case PrimitiveOp.I64Add => List(Instruction.I64Add)
    case PrimitiveOp.I32Add => List(Instruction.I32Add)
    case PrimitiveOp.I64Mul => List(Instruction.I64Mul)
    case PrimitiveOp.I32Mul => List(Instruction.I32Mul)
    case PrimitiveOp.I32Println => List(
      Instruction.Call(Symbol.I32Println),
      Instruction.I32Const(0)
    )
    case PrimitiveOp.I32Read => List(Instruction.Call(Symbol.I32Read))
    case PrimitiveOp.Sorry => assert(false, "program contains `sorry`")
    // case _ => assert(false, s"Not supported: $op")

  def nameEncode(name: String): String =
    name.replaceAll(" ", "_").filter(ch => ch != '(' && ch != ')')

  def computeFuncType(tpe: Expr.Type, isClosure: Boolean = true)(using Context): FuncType = tpe match
    case Expr.Type.TermArrow(params, result) =>
      var paramTypes = params.map(binder => translateType(binder.tpe))
      if isClosure then
        // the first param is the closure pointer
        paramTypes = ValType.AnyRef :: paramTypes
      FuncType(paramTypes, Some(translateType(result)))
    case Expr.Type.Capturing(inner, _) => computeFuncType(inner)
    case _ => assert(false, s"Unsupported type for computing func type: $tpe")

  def createFuncParams(params: List[Expr.Binder.TermBinder])(using Context): List[(Symbol, ValType)] =
    params.map: binder =>
      val paramName = binder.name
      val paramType = translateType(binder.tpe)
      (Symbol.fresh(paramName), paramType)
  
  def newLocalsScope[R](op: Context ?=> R)(using Context): R =
    val oldLocals = ctx.locals
    val ctx1 = ctx.copy(locals = ArrayBuffer.empty)
    op(using ctx1)

  def createClosureTypes(funcType: FuncType)(using Context): ClosureTypeInfo =
    ctx.closureTypes.get(funcType) match
      case None => 
        val typeName = 
          nameEncode(funcType.paramTypes.map(_.show).mkString("_") + " to " + funcType.resultType.get.show)
        val closName = s"clos_$typeName"
        val funcSymm = Symbol.fresh(typeName)
        val closSymm = Symbol.fresh(closName)
        val closType = StructType(
          List(
            (Symbol.Function, ValType.TypedRef(funcSymm)),
          ),
          subClassOf = None
        )
        emitType(TypeDef(funcSymm, funcType))
        emitType(TypeDef(closSymm, closType))
        val result = ClosureTypeInfo(funcSymm, closSymm)
        ctx.closureTypes += (funcType -> result)
        result
      case Some(info) => info

  /** What is the WASM type of the WASM representation of a value of this type? */
  def translateType(tpe: Expr.Type)(using Context): ValType = tpe match
    case Expr.Type.Base(Expr.BaseType.I64) => ValType.I64
    case Expr.Type.Base(Expr.BaseType.I32) => ValType.I32
    case Expr.Type.Base(Expr.BaseType.UnitType) => ValType.I32
    case Expr.Type.Capturing(inner, _) => translateType(inner)
    case Expr.Type.TermArrow(params, result) =>
      val funcType = computeFuncType(tpe)
      val info = createClosureTypes(funcType)
      ValType.TypedRef(info.closTypeSym)
    case _ => assert(false, s"Unsupported type: $tpe")

  def dropLocalBinders(xs: Set[Int], numLocals: Int): Set[Int] =
    xs.flatMap: idx =>
      if idx >= numLocals then
        Some(idx - numLocals)
      else
        None

  def freeLocalBinders(t: Expr.Term)(using Context): Set[Int] = t match
    case Term.BinderRef(idx) => Set(idx)
    case Term.SymbolRef(sym) => Set.empty
    case Term.StrLit(value) => Set.empty
    case Term.IntLit(value) => Set.empty
    case Term.UnitLit() => Set.empty
    case Term.TermLambda(params, body) =>
      dropLocalBinders(freeLocalBinders(body), params.size)
    case Term.TypeLambda(params, body) =>
      dropLocalBinders(freeLocalBinders(body), params.size)
    case Term.Bind(binder, recursive, bound, body) =>
      val boundFree = freeLocalBinders(bound)
      val boundFree1 = if recursive then dropLocalBinders(boundFree, 1) else boundFree
      val bodyFree = dropLocalBinders(freeLocalBinders(body), 1)
      boundFree1 ++ bodyFree
    case Term.PrimOp(op, args) => args.flatMap(freeLocalBinders).toSet
    case Term.Apply(fun, args) => freeLocalBinders(fun) ++ args.flatMap(freeLocalBinders)
    case Term.TypeApply(term, targs) => freeLocalBinders(term)
    case Term.Select(base, fieldInfo) => freeLocalBinders(base)
    case Term.Assign(lhs, rhs) => freeLocalBinders(lhs) ++ freeLocalBinders(rhs)
    case Term.StructInit(sym, args) => args.flatMap(freeLocalBinders).toSet

  def translateClosure(
    funType: Expr.Type,   // the type of the source function
    params: List[Expr.Binder.TermBinder],   // parameters of the source function
    body: Expr.Term,   // body of the source function
    selfBinder: Option[Expr.Binder]  // whether the source function is self-recursive
  )(using Context): List[Instruction] =
    val funcType = computeFuncType(funType)
    val closureInfo = createClosureTypes(funcType)
    val funName: String = selfBinder match
      case Some(bd) => bd.name
      case None => "anonfun"
    val freeVars = dropLocalBinders(freeLocalBinders(body), params.size)
    // (1) create the exact closure type
    val depVarsSet = 
      // local variables this closure depends on other than itself
      if selfBinder.isDefined then dropLocalBinders(freeVars, 1) else freeVars
    val depVars = depVarsSet.toList
    val envMap: Map[Int, (Symbol, ValType)] = Map.from:
      depVars.map: idx =>
        val binder = ctx.binderInfos(idx).binder.asInstanceOf[Expr.Binder.TermBinder]
        val sym = Symbol.fresh(binder.name)
        val tpe = translateType(binder.tpe)
        (idx, (sym, tpe))
    val fields = (Symbol.Function -> ValType.TypedRef(closureInfo.funcTypeSym)) :: depVars.map(idx => envMap(idx))
    val exactClosureType = StructType(fields, subClassOf = Some(closureInfo.closTypeSym))
    val exactClosureTypeName = s"clos_${funName}"
    val exactClosureTypeSym = Symbol.fresh(exactClosureTypeName)
    emitType(TypeDef(exactClosureTypeSym, exactClosureType))
    // (2) create the worker function
    val workerName = s"worker_${funName}"
    val workerSym = Symbol.fresh(workerName)
    val selfParamSym = Symbol.fresh("self")
    val selfCastedSym = Symbol.fresh("self_casted")
    val localMap: Map[Int, Symbol] = Map.from:
      depVars.map: idx =>
        val binder = ctx.binderInfos(idx).binder.asInstanceOf[Expr.Binder.TermBinder]
        val sym = Symbol.fresh(s"local_${binder.name}")
        (idx, sym)
    def funcLocals = depVars.map: idx =>
      val tpe = envMap(idx)._2
      val sym = localMap(idx)
      (sym, tpe)
    val selfCastInstrs = List(
      Instruction.LocalGet(selfParamSym),
      Instruction.RefCast(exactClosureTypeSym),
      Instruction.LocalSet(selfCastedSym)
    )
    val setLocalInstrs = depVars.flatMap: idx =>
      List(
        Instruction.LocalGet(selfCastedSym),
        Instruction.StructGet(exactClosureTypeSym, envMap(idx)._1),
        Instruction.LocalSet(localMap(idx))
      )
    val workerFunParams = (selfParamSym -> ValType.AnyRef) :: createFuncParams(params)
    val workerFunc: Wasm.Func =
      newLocalsScope:
        emitLocal(selfCastedSym, ValType.TypedRef(exactClosureTypeSym))
        emitLocals(funcLocals)
        val newBinderInfos: List[BinderInfo] = ctx.binderInfos.zipWithIndex.map: (binderInfo, idx) =>
          if depVarsSet `contains` idx then
            BinderInfo.Sym(binderInfo.binder, localMap(idx))
          else BinderInfo.Inaccessible(binderInfo.binder)
        var ctx1 = ctx.usingBinderInfos(newBinderInfos)
        selfBinder match
          case Some(binder) =>
            ctx1 = ctx1.withLocalSym(binder, selfCastedSym)
          case None =>
        (params `zip` workerFunParams.tail).foreach: 
          case (binder, (sym, _)) =>
            ctx1 = ctx1.withLocalSym(binder, sym)
        val bodyInstrs = genTerm(body)(using ctx1)
        val funcResultType = funcType.resultType
        Func(
          workerSym, 
          workerFunParams, 
          funcResultType,
          locals = finalizeLocals,
          body = selfCastInstrs ++ setLocalInstrs ++ bodyInstrs
        )
    emitFunc(workerFunc)
    emitElemDeclare(ExportKind.Func, workerSym)
    val getFuncInstrs = List(Instruction.RefFunc(workerSym))
    val getEnvInstrs = depVars.flatMap: idx =>
      genBinderRef(idx)
    val createStructInstrs = List(
      Instruction.StructNew(exactClosureTypeSym)
    )
    getFuncInstrs ++ getEnvInstrs ++ createStructInstrs

  def genBinderRef(binder: Int)(using Context): List[Instruction] =
    ctx.binderInfos(binder) match
      case BinderInfo.Sym(binder, sym) => List(Instruction.LocalGet(sym))
      case BinderInfo.Inaccessible(binder) => assert(false, s"Inaccessible binder: $binder")

  def genTerm(t: Expr.Term)(using Context): List[Instruction] = t match
    case Term.IntLit(value) => 
      translateType(t.tpe) match
        case ValType.I32 => List(Instruction.I32Const(value))
        case ValType.I64 => List(Instruction.I64Const(value))
        case _ => assert(false, s"Unsupported type for int literal: ${t.tpe}")
    case Term.PrimOp(op, args) =>
      val argInstrs = args.flatMap(genTerm)
      argInstrs ++ translatePrimOp(op)
    case Term.TermLambda(params, body) =>
      translateClosure(t.tpe, params, body, selfBinder = None)
    case Term.Bind(binder, isRecursive, Term.TermLambda(params, body), expr) =>
      val localSym = Symbol.fresh(binder.name)
      val localType = translateType(binder.tpe)
      emitLocal(localSym, localType)
      val closureInstrs = translateClosure(binder.tpe, params, body, if isRecursive then Some(binder) else None)
      val setLocalInstrs = List(Instruction.LocalSet(localSym))
      val bodyInstrs = genTerm(expr)(using ctx.withLocalSym(binder, localSym))
      closureInstrs ++ setLocalInstrs ++ bodyInstrs
    case Term.Bind(binder, false, bound, body) =>
      val localSym = Symbol.fresh(binder.name)
      val localType = translateType(binder.tpe)
      emitLocal(localSym, localType)
      val boundInstrs = genTerm(bound)
      val bodyInstrs = genTerm(body)(using ctx.withLocalSym(binder, localSym))
      boundInstrs ++ List(Instruction.LocalSet(localSym)) ++ bodyInstrs
    case Term.BinderRef(idx) => genBinderRef(idx)
    case Term.SymbolRef(sym) =>
      val info = ctx.defInfos(sym)
      info match
        case DefInfo.FuncDef(_, workerSym) =>
          val funcType = computeFuncType(t.tpe, isClosure = true)
          val closureInfo = createClosureTypes(funcType)
          val getSelfInstrs = List(
            Instruction.RefFunc(workerSym),
          )
          val createClosureInstrs = List(
            Instruction.StructNew(closureInfo.closTypeSym),
          )
          getSelfInstrs ++ createClosureInstrs
        case DefInfo.GlobalDef(globalSym) =>
          List(Instruction.GlobalGet(globalSym))
    case Term.Apply(Term.SymbolRef(sym), args) =>
      val DefInfo.FuncDef(funcSym, _) = ctx.defInfos(sym): @unchecked
      val argInstrs = args.flatMap(genTerm)
      val callInstrs = List(Instruction.Call(funcSym))
      argInstrs ++ callInstrs
    case Term.Apply(fun, args) =>
      val localSym = Symbol.fresh("fun")
      val funcType = computeFuncType(fun.tpe)
      val info = createClosureTypes(funcType)
      emitLocal(localSym, ValType.TypedRef(info.closTypeSym))
      val funInstrs = genTerm(fun) ++ List(Instruction.LocalSet(localSym))
      val getWorkerInstrs = List(
        Instruction.LocalGet(localSym),
        Instruction.StructGet(info.closTypeSym, Symbol.Function),
      )
      val getSelfArgInstrs = List(Instruction.LocalGet(localSym))
      val argInstrs = args.flatMap(genTerm)
      val callRefInstrs = List(Instruction.CallRef(info.funcTypeSym))
      funInstrs ++ getSelfArgInstrs ++ argInstrs ++ getWorkerInstrs ++ callRefInstrs
    case _ => assert(false, s"Don't know how to translate this term: $t")

  def genModuleFunction(funType: Expr.Type, funSymbol: Symbol, workerSymbol: Symbol, expr: Expr.Term)(using Context): Unit =
    val Term.TermLambda(ps, body) = expr: @unchecked
    val funcType = computeFuncType(funType, isClosure = false)
    val workerType = computeFuncType(funType, isClosure = true)
    val paramSymbols = ps.map(p => Symbol.fresh(p.name))
    val funcParams = paramSymbols `zip` funcType.paramTypes
    val workerSelfSymbol = Symbol.fresh("self")
    val workerParams = (workerSelfSymbol -> ValType.AnyRef) :: funcParams
    val func =
      newLocalsScope:
        val binderInfos = (ps `zip` paramSymbols).map: (bd, sym) =>
          BinderInfo.Sym(bd, sym)
        val ctx1 = ctx.usingBinderInfos(binderInfos.reverse)
        val bodyInstrs = genTerm(body)(using ctx1)
        Func(
          funSymbol,
          funcParams,
          funcType.resultType,
          locals = finalizeLocals,
          body = bodyInstrs,
        )
    val workerFunc =
      val getParamsInstrs =
        workerParams.tail.map: (sym, _) =>
          Instruction.LocalGet(sym)
      val callFuncInstrs = List(Instruction.Call(funSymbol))
      Func(
        workerSymbol,
        workerParams,
        funcType.resultType,
        locals = List(),
        body = getParamsInstrs ++ callFuncInstrs
      )
    emitFunc(func)
    emitFunc(workerFunc)

  def isValidMain(d: Expr.Definition)(using Context): Boolean = d match
    case Definition.ValDef(sym, body) if sym.name == "main" =>
      val mainType = Expr.Type.TermArrow(Nil, Expr.Type.Base(Expr.BaseType.I32))
      val mainFuncType = computeFuncType(mainType)
      val defType = sym.tpe
      computeFuncType(defType) == mainFuncType
    case _ => false

  def toNullable(tp: ValType): ValType = tp match
    case ValType.TypedRef(sym, _) => ValType.TypedRef(sym, nullable = true)
    case _ => tp

  def makeDefaultValue(tp: ValType): Instruction = tp match
    case ValType.I32 => Instruction.I32Const(0)
    case ValType.I64 => Instruction.I64Const(0)
    case ValType.AnyRef => Instruction.RefNullAny
    case ValType.TypedRef(sym, nullable) if nullable => Instruction.RefNull(sym)
    case _ => assert(false, s"Unsupported type for making default value: $tp")

  def emitDefaultImports()(using Context): Unit =
    emitImportFunc(ImportFunc("", "", Symbol.I32Println, I32PrintlnType))
    emitImportFunc(ImportFunc("", "", Symbol.I32Read, I32ReadType))

  def genModule(m: Expr.Module)(using Context): Unit =
    val mainSym = m.defns.find(isValidMain) match
      case Some(Definition.ValDef(sym, _)) => sym
      case Some(_) => assert(false, "Invalid definition")
      case None => assert(false, s"No valid main function in module")
    // First of all, emit imports
    emitDefaultImports()
    // (1) create symbols for all the definitions
    m.defns.foreach: defn =>
      defn match
        case Definition.ValDef(sym, body) =>
          body match
            case body: Term.TermLambda =>
              val funType = computeFuncType(sym.tpe)
              val funcSym = Symbol.fresh(sym.name)
              val workerSym = Symbol.fresh(s"worker_${sym.name}")
              emitElemDeclare(ExportKind.Func, workerSym)
              ctx.defInfos += (sym -> DefInfo.FuncDef(funcSym, workerSym))
            case _ =>
              val globalSym = Symbol.fresh(sym.name)
              ctx.defInfos += (sym -> DefInfo.GlobalDef(globalSym))
        case Definition.StructDef(sym) =>
          assert(false, s"Not supported yet: ${sym}")
    // (2) emit func definitions
    m.defns.foreach: defn =>
      defn match
        case Definition.ValDef(sym, body) =>
          body match
            case body: Term.TermLambda =>
              val DefInfo.FuncDef(funcSym, workerSym) = ctx.defInfos(sym): @unchecked
              genModuleFunction(sym.tpe, funcSym, workerSym, body)
            case _ =>
        case _ =>
    // (3) emit global definitions
    m.defns.foreach: defn =>
      defn match
        case Definition.ValDef(sym, body) =>
          body match
            case body: Term.TermLambda =>
            case body =>
              val valType = toNullable(translateType(sym.tpe))
              val defaultVal = makeDefaultValue(valType)
              val DefInfo.GlobalDef(globalSym) = ctx.defInfos(sym): @unchecked
              val g = Global(globalSym, valType, mutable = true, defaultVal)
              emitGlobal(g)
        case _ =>
    // (4) emit the start function
    val startFuncSymbol = Symbol.fresh("init")
    val startFunc =
      newLocalsScope:
        val instrs =
          m.defns.flatMap: defn =>
            defn match
              case Definition.ValDef(sym, body) =>
                body match
                  case body: Term.TermLambda => Nil
                  case body =>
                    val valType = toNullable(translateType(sym.tpe))
                    val DefInfo.GlobalDef(globalSym) = ctx.defInfos(sym): @unchecked
                    val bodyInstrs = genTerm(body)
                    val setGlobalInstrs = List(
                      Instruction.GlobalSet(globalSym)
                    )
                    bodyInstrs ++ setGlobalInstrs
              case _ => Nil
        Func(
          startFuncSymbol,
          List(),
          None,
          finalizeLocals,
          instrs
        )
    ctx.startFunc = Some(startFuncSymbol)
    emitFunc(startFunc)
    // Lastly, emit the main function
    val DefInfo.FuncDef(mainFunc, _) = ctx.defInfos(mainSym): @unchecked
    val exp = Export("entrypoint", ExportKind.Func, mainFunc)
    emitExport(exp)

  def finalize(using Context): Module =
    Module(
      ctx.declares.toList ++ 
        ctx.imports.toList ++ 
        ctx.types.toList ++ 
        ctx.globals.toList ++
        ctx.funcs.toList ++ 
        ctx.exports.toList ++
        ctx.startFunc.map(Start(_)).toList
    )
