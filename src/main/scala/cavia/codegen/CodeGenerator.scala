package cavia
package codegen

import core.*
import ast.{Expr, Wasm}
import cavia.core.ast.Expr.Term
import Wasm.*
import typechecking.*
import scala.collection.mutable.ArrayBuffer
import cavia.core.ast.Expr.PrimitiveOp

object CodeGenerator:
  case class Context(
    funcs: ArrayBuffer[Func] = ArrayBuffer.empty,
    exports: ArrayBuffer[Export] = ArrayBuffer.empty,
    locals: ArrayBuffer[(Symbol, ValType)] = ArrayBuffer.empty,
    localSyms: List[Symbol] = Nil,
  ):
    def withLocalSym(sym: Symbol): Context =
      copy(localSyms = sym :: localSyms)

  def ctx(using ctx: Context): Context = ctx

  def emitFunc(f: Func)(using Context): Unit =
    ctx.funcs += f

  def emitExport(e: Export)(using Context): Unit =
    ctx.exports += e

  def emitLocal(sym: Symbol, tpe: ValType)(using Context): Unit =
    ctx.locals += (sym -> tpe)

  def translatePrimOp(op: PrimitiveOp)(using Context): List[Instruction] = op match
    case PrimitiveOp.I64Add => List(Instruction.I64Add)
    case _ => assert(false, s"Not supported: $op")

  def translateType(tpe: Expr.Type)(using Context): ValType = tpe match
    case Expr.Type.Base(Expr.BaseType.I64) => ValType.I64
    case _ => assert(false, s"Unsupported type: $tpe")

  def genTerm(t: Expr.Term)(using Context): List[Instruction] = t match
    case Term.IntLit(value) => List(Instruction.I64Const(value))
    case Term.PrimOp(op, args) =>
      val argInstrs = args.flatMap(genTerm)
      argInstrs ++ translatePrimOp(op)
    case Term.Bind(binder, bound, body) =>
      val localSym = Symbol.fresh(binder.name)
      val localType = translateType(binder.tpe)
      emitLocal(localSym, localType)
      val boundInstrs = genTerm(bound)
      val bodyInstrs = genTerm(body)(using ctx.withLocalSym(localSym))
      boundInstrs ++ List(Instruction.LocalSet(localSym)) ++ bodyInstrs
    case Term.BinderRef(idx) =>
      val localSym = ctx.localSyms(idx)
      List(Instruction.LocalGet(localSym))
    case _ => assert(false, s"Not supported: $t")

  def genModule(m: Expr.Module)(using Context): Unit = 
    m.defns match
      case (d: Expr.Definition.ValDef) :: Nil =>
        val mainType = Expr.Type.TermArrow(Nil, Expr.Type.Base(Expr.BaseType.I64))
        given ctx: TypeChecker.Context = TypeChecker.Context.empty
        if TypeComparer.checkSubtype(d.tpe, mainType) then
          val Term.TermLambda(Nil, body) = d.body: @unchecked
          val insts = genTerm(body)
          val func = Func(Symbol.fresh(d.sym.name), params = Nil, result = ValType.I64, locals = Nil, insts)
          emitFunc(func)
          val exp = Export("entrypoint", ExportKind.Func, func.ident)
          emitExport(exp)
        else assert(false, s"Incompatible type: ${d.tpe}")
      case _ => assert(false, s"Not supported: $m")

  def finalize(using Context): Module =
    Module(ctx.funcs.toList ++ ctx.exports.toList)
