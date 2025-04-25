package cavia
package core
package ast

import tokenizing.*

object Syntax:
  case class TermParam(name: String, tpe: Type) extends Positioned
  case class TypeParam(name: String, bound: Option[Type]) extends Positioned
  case class CaptureParam(name: String, bound: Option[CaptureSet]) extends Positioned
  case class CaptureRef(name: String) extends Positioned
  case class CaptureSet(elems: List[CaptureRef]) extends Positioned

  enum InfixOp extends Positioned:
    case Plus, Minus, Mul, Div, Mod, Concat
    case Eq, Neq, Lt, Gt, Lte, Gte
    case And, Or

  enum PrefixOp extends Positioned:
    case Neg, Not

  enum Term extends Positioned:
    case Ident(name: String)
    case Select(base: Term, field: String)
    case StrLit(value: String)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case UnitLit()
    case Lambda(params: List[TermParam], body: Term)
    case TypeLambda(params: List[TypeParam | CaptureParam], body: Term)
    //case CaptureLambda(params: List[CaptureParam], body: Term)
    case Apply(fun: Term, args: List[Term])
    case TypeApply(term: Term, targs: List[Type | CaptureSet])
    //case CaptureApply(term: Term, captures: List[CaptureSet])
    case Block(stmts: List[Definition | Term])
    case Assign(lhs: Term, rhs: Term)
    case Infix(op: InfixOp, lhs: Term, rhs: Term)
    case Prefix(op: PrefixOp, term: Term)
    case If(cond: Term, thenBranch: Term, elseBranch: Option[Term])
  import Term.*

  case class TypeParamList(params: List[TypeParam | CaptureParam]) extends Positioned
  case class TermParamList(params: List[TermParam]) extends Positioned

  case class FieldDef(name: String, isVar: Boolean, tpe: Type) extends Positioned

  enum Definition extends Positioned:
    case ValDef(name: String, tpe: Option[Type], expr: Term)
    case DefDef(name: String, captureSet: Option[CaptureSet], paramss: List[TypeParamList | TermParamList], resultType: Option[Type], expr: Term)
    case StructDef(name: String, targs: List[TypeParam | CaptureParam], fields: List[FieldDef])

    val name: String
  import Definition.*

  type ValueDef = ValDef | DefDef

  enum Type extends Positioned:
    case Ident(name: String)
    case Arrow(params: List[TermParam], result: Type)
    case TypeArrow(params: List[TypeParam | CaptureParam], result: Type)
    //case CaptureArrow(params: List[CaptureParam], result: Type)
    case Capturing(inner: Type, captureSet: CaptureSet)
    case AppliedType(tycon: Type, args: List[Type])
  import Type.*
