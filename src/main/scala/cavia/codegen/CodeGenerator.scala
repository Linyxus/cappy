package cavia
package codegen

import core.*
import ast.{Expr, Wasm}
import cavia.core.ast.Expr.Term
import Wasm.*
import typechecking.*
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import cavia.core.ast.Expr.PrimitiveOp

object CodeGenerator:
  case class ClosureTypeInfo(funcTypeSym: Symbol, closTypeSym: Symbol)

  enum BinderInfo:
    case Sym(binder: Expr.Binder, sym: Symbol)
    case Inaccessible(binder: Expr.Binder)

    val binder: Expr.Binder

  case class Context(
    funcs: ArrayBuffer[Func] = ArrayBuffer.empty,
    exports: ArrayBuffer[Export] = ArrayBuffer.empty,
    locals: ArrayBuffer[(Symbol, ValType)] = ArrayBuffer.empty,
    types: ArrayBuffer[TypeDef] = ArrayBuffer.empty,
    closureTypes: Map[FuncType, ClosureTypeInfo] = Map.empty,
    binderInfos: List[BinderInfo] = Nil
  ):
    def withLocalSym(binder: Expr.Binder, sym: Symbol): Context =
      copy(binderInfos = BinderInfo.Sym(binder, sym) :: binderInfos)

  def ctx(using ctx: Context): Context = ctx

  def emitFunc(f: Func)(using Context): Unit =
    ctx.funcs += f

  def emitExport(e: Export)(using Context): Unit =
    ctx.exports += e

  def emitLocal(sym: Symbol, tpe: ValType)(using Context): Unit =
    ctx.locals += (sym -> tpe)

  def emitType(t: TypeDef)(using Context): Unit =
    ctx.types += t

  def finalizeLocals(using Context): List[(Symbol, ValType)] =
    val result = ctx.locals.toList
    ctx.locals.clear()
    result

  def translatePrimOp(op: PrimitiveOp)(using Context): List[Instruction] = op match
    case PrimitiveOp.I64Add => List(Instruction.I64Add)
    case PrimitiveOp.I32Add => List(Instruction.I32Add)
    case PrimitiveOp.I64Mul => List(Instruction.I64Mul)
    case PrimitiveOp.I32Mul => List(Instruction.I32Mul)
    // case _ => assert(false, s"Not supported: $op")

  def computeFuncType(tpe: Expr.Type)(using Context): FuncType = tpe match
    case Expr.Type.TermArrow(params, result) =>
      FuncType(ValType.AnyRef :: params.map(binder => translateType(binder.tpe)), translateType(result))
        // the first param is the closure pointer
    case Expr.Type.Capturing(inner, _) => computeFuncType(inner)
    case _ => assert(false, s"Unsupported type for computing func type: $tpe")

  def createClosureTypes(funcType: FuncType)(using Context): ClosureTypeInfo =
    ctx.closureTypes.get(funcType) match
      case None => 
        val typeName = 
          s"${funcType.paramTypes.map(_.show).mkString("_")} to ${funcType.resultType.show}"
          .replaceAll(" ", "_")
          .filter(ch => ch != '(' && ch != ')')
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
  
  def translateClosure(funType: Expr.Type, params: List[Expr.Binder.TermBinder], body: Expr.Term, selfBinder: Option[Expr.Binder])(using Context): List[Instruction] =
    val funcType = computeFuncType(funType)
    val closureInfo = createClosureTypes(funcType)
    ???

  def genTerm(t: Expr.Term)(using Context): List[Instruction] = t match
    case Term.IntLit(value) => 
      translateType(t.tpe) match
        case ValType.I32 => List(Instruction.I32Const(value))
        case ValType.I64 => List(Instruction.I64Const(value))
        case _ => assert(false, s"Unsupported type for int literal: ${t.tpe}")
    case Term.PrimOp(op, args) =>
      val argInstrs = args.flatMap(genTerm)
      argInstrs ++ translatePrimOp(op)
    case Term.Bind(binder, isRecursive, Term.TermLambda(params, body), expr) =>
      val funcType = computeFuncType(binder.tpe)
      val closureInfo = createClosureTypes(funcType)
      translateClosure(binder.tpe, params, body, if isRecursive then Some(binder) else None)
    case Term.Bind(binder, false, bound, body) =>
      val localSym = Symbol.fresh(binder.name)
      val localType = translateType(binder.tpe)
      emitLocal(localSym, localType)
      val boundInstrs = genTerm(bound)
      val bodyInstrs = genTerm(body)(using ctx.withLocalSym(binder, localSym))
      boundInstrs ++ List(Instruction.LocalSet(localSym)) ++ bodyInstrs
    case Term.BinderRef(idx) =>
      val binderInfo = ctx.binderInfos(idx)
      binderInfo match
        case BinderInfo.Sym(binder, sym) => List(Instruction.LocalGet(sym))
        case BinderInfo.Inaccessible(binder) => assert(false, s"Inaccessible binder: $binder")
    case _ => assert(false, s"Not supported: $t")

  def genModule(m: Expr.Module)(using Context): Unit = 
    m.defns match
      case (d: Expr.Definition.ValDef) :: Nil =>
        val mainType = Expr.Type.TermArrow(Nil, Expr.Type.Base(Expr.BaseType.I32))
        given TypeChecker.Context = TypeChecker.Context.empty
        if TypeComparer.checkSubtype(d.tpe, mainType) then
          val Term.TermLambda(Nil, body) = d.body: @unchecked
          val insts = genTerm(body)
          val func = Func(Symbol.fresh(d.sym.name), params = Nil, result = ValType.I32, locals = finalizeLocals, insts)
          emitFunc(func)
          val exp = Export("entrypoint", ExportKind.Func, func.ident)
          emitExport(exp)
        else assert(false, s"Incompatible type: ${d.tpe}")
      case _ => assert(false, s"Not supported: $m")

  def finalize(using Context): Module =
    Module(ctx.types.toList ++ ctx.funcs.toList ++ ctx.exports.toList)
