package cavia
package codegen

import core.*
import ast.{Expr, Wasm}
import cavia.core.ast.Expr.Term
import Wasm.*
import typechecking.*
import scala.collection.mutable.ArrayBuffer

object CodeGenerator:
  case class Context(funcs: ArrayBuffer[Func] = ArrayBuffer.empty, exports: ArrayBuffer[Export] = ArrayBuffer.empty)

  def ctx(using ctx: Context): Context = ctx

  def emitFunc(f: Func)(using Context): Unit =
    ctx.funcs += f

  def emitExport(e: Export)(using Context): Unit =
    ctx.exports += e

  def genTerm(t: Expr.Term)(using Context): List[Instruction] = t match
    case Term.IntLit(value) => List(Instruction.I64Const(value))
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
