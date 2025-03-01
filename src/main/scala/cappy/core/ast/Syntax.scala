package cappy
package core
package ast

import tokenizing.*

object Syntax:
  case class TermParam(name: String, tpe: Type) extends Positioned
  case class TypeParam(name: String, bound: Option[Type]) extends Positioned
  case class CaptureParam(name: String) extends Positioned
  case class CaptureRef(name: String) extends Positioned
  case class CaptureSet(elems: List[CaptureRef]) extends Positioned

  enum Term extends Positioned:
    case Ident(name: String)
    case StrLit(value: String)
    case Lambda(params: List[TermParam], body: Term)
    case TypeLambda(params: List[TypeParam], body: Type)
    case CaptureLambda(params: List[CaptureParam], body: Term)
    case Apply(fun: Term, args: List[Term])
    case TypeApply(term: Term, targs: List[Type])
    case CaptureApply(term: Term, captures: List[CaptureRef])

  enum Definition extends Positioned:
    case ValDef(name: String, tpe: Option[Type], expr: Term)

  enum Type extends Positioned:
    case Ident(name: String)
    case Arrow(params: List[TermParam], result: Type)
    case TypeArrow(params: List[TypeParam], result: Type)
    case CaptureArrow(params: List[CaptureParam], result: Type)
    case Capturing(inner: Type, captureSet: CaptureSet)
    case AppliedType(tycon: Type, args: List[Type])
