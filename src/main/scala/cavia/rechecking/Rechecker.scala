package cavia
package rechecking

import core.*
import ast.Expr
import typechecking.*
import TypeChecker.Context

abstract class Rechecker:
  import Expr.*
  import TypeChecker.ctx

  def recheckBinderRef(old: Term, idx: Int)(using Context): Term = old

  def recheckSymbolRef(old: Term, sym: DefSymbol)(using Context): Term = old

  def recheckStrLit(old: Term, value: String)(using Context): Term = old

  def recheckIntLit(old: Term, value: Int)(using Context): Term = old

  def recheckBoolLit(old: Term, value: Boolean)(using Context): Term = old

  def recheckCharLit(old: Term, value: Char)(using Context): Term = old

  def recheckUnitLit(old: Term)(using Context): Term = old

  def recheckTermLambda(old: Term, params: List[Binder.TermBinder], body: Term, skolemizedBinders: List[Binder.TermBinder])(using Context): Term =
    val ctx1 = ctx.extend(params)
    val body1 = recheck(body)(using ctx1)
    old.derivedTermLambda(params, body1, skolemizedBinders)

  def recheckTypeLambda(old: Term, params: List[Binder.TypeBinder | Binder.CaptureBinder], body: Term)(using Context): Term =
    val ctx1 = ctx.extend(params)
    val body1 = recheck(body)(using ctx1)
    old.derivedTypeLambda(params, body1)

  def recheckBind(old: Term, binder: Binder.TermBinder, recursive: Boolean, bound: Term, body: Term)(using Context): Term =
    val ctx1 = ctx.extend(binder)
    val bound1 = 
      if recursive then recheck(bound)(using ctx1)
      else recheck(bound)
    val body1 = recheck(body)(using ctx1)
    old.derivedBind(binder, recursive, bound1, body1)

  def recheckPrimOp(old: Term, op: PrimitiveOp, targs: List[Type], args: List[Term])(using Context): Term =
    val args1 = args.map(recheck(_))
    old.derivedPrimOp(op, targs, args1)

  def recheckStructInit(old: Term, sym: StructSymbol, targs: List[Type | CaptureSet], args: List[Term])(using Context): Term =
    val args1 = args.map(recheck(_))
    old.derivedStructInit(sym, targs, args1)

  def recheckApply(old: Term, fun: Term, args: List[Term])(using Context): Term =
    val fun1 = recheck(fun)
    val args1 = args.map(recheck(_))
    old.derivedApply(fun1, args1)

  def recheckTypeApply(old: Term, term: Term, targs: List[Type | CaptureSet])(using Context): Term =
    val term1 = recheck(term)
    old.derivedTypeApply(term1, targs)

  def recheckSelect(old: Term, base: Term, fieldInfo: FieldInfo)(using Context): Term =
    val base1 = recheck(base)
    old.derivedSelect(base1, fieldInfo)

  def recheckIf(old: Term, cond: Term, thenBranch: Term, elseBranch: Term)(using Context): Term =
    val cond1 = recheck(cond)
    val thenBranch1 = recheck(thenBranch)
    val elseBranch1 = recheck(elseBranch)
    old.derivedIf(cond1, thenBranch1, elseBranch1)

  def recheckResolveExtension(old: Term, sym: ExtensionSymbol, targs: List[Type | CaptureSet], methodName: String)(using Context): Term = old

  def recheckPattern(old: Pattern)(using Context): Pattern = old

  def recheckMatchCase(old: MatchCase, pat: Pattern, body: Term)(using Context): MatchCase =
    val pat1 = recheckPattern(pat)
    val body1 = recheck(body)
    old.derivedMatchCase(pat1, body1)

  def recheckMatch(old: Term, scrutinee: Term, cases: List[Expr.MatchCase])(using Context): Term =
    val scrutinee1 = recheck(scrutinee)
    val cases1 = cases.map: cas =>
      recheckMatchCase(cas, cas.pat, cas.body)
    old.derivedMatch(scrutinee1, cases1)

  def recheck(expr: Term)(using Context): Expr.Term = expr match
    case Term.BinderRef(idx) => recheckBinderRef(expr, idx)
    case Term.SymbolRef(sym) => recheckSymbolRef(expr, sym)
    case Term.StrLit(value) => recheckStrLit(expr, value)
    case Term.IntLit(value) => recheckIntLit(expr, value)
    case Term.BoolLit(value) => recheckBoolLit(expr, value)
    case Term.CharLit(value) => recheckCharLit(expr, value)
    case Term.UnitLit() => recheckUnitLit(expr)
    case Term.TermLambda(params, body, skolemizedBinders) => recheckTermLambda(expr, params, body, skolemizedBinders)
    case Term.TypeLambda(params, body) => recheckTypeLambda(expr, params, body)
    case Term.Bind(binder, recursive, bound, body) => recheckBind(expr, binder, recursive, bound, body)
    case Term.PrimOp(op, targs, args) => recheckPrimOp(expr, op, targs, args)
    case Term.StructInit(sym, targs, args) => recheckStructInit(expr, sym, targs, args)
    case Term.Apply(fun, args) => recheckApply(expr, fun, args)
    case Term.TypeApply(term, targs) => recheckTypeApply(expr, term, targs)
    case Term.Select(base, fieldInfo) => recheckSelect(expr, base, fieldInfo)
    case Term.If(cond, thenBranch, elseBranch) => recheckIf(expr, cond, thenBranch, elseBranch)
    case Term.ResolveExtension(sym, targs, methodName) => recheckResolveExtension(expr, sym, targs, methodName)
    case Term.Match(scrutinee, cases) => recheckMatch(expr, scrutinee, cases)
