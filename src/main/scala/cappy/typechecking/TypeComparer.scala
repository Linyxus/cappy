package cappy
package typechecking

import core.*
import ast.*

object TypeComparer:
  import TypeChecker.*
  import Expr.*
  import Binder.*

  def checkSubcapture(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean =
    cs1.elems.forall(checkSubcapture(_, cs2))

  def checkSubcapture(x1: CaptureRef, cs2: CaptureSet)(using Context): Boolean =
    cs2.elems.contains(x1) || {
      x1 match
        case CaptureRef.BinderRef(idx) => getBinder(idx) match
          case Binder.TermBinder(name, tpe) => checkSubcapture(tpe.captureSet, cs2)
          case Binder.CaptureBinder(name, bound) => checkSubcapture(bound, cs2)
          case _: Binder.TypeBinder => assert(false, "binder kind is absurd")
        case CaptureRef.SymbolRef(sym) => checkSubcapture(sym.tpe.captureSet, cs2)
        case CaptureRef.CAP() => false
    }

  def checkSubtype(tp1: Type, tp2: Type)(using Context): Boolean =
    (tp1, tp2) match
      case _ if tp1 == tp2 => true
      case (_, Type.Base(BaseType.AnyType)) => true
      case (Type.BinderRef(idx1), tp2) => getBinder(idx1) match
        case Binder.TypeBinder(name, bound) => checkSubtype(bound, tp2)
        case _ => assert(false, "binder kind is absurd")
      case (Type.Capturing(inner, captureSet), tp2) =>
        checkSubcapture(captureSet, tp2.captureSet) && checkSubtype(inner, tp2)
      case (tp1, Type.Capturing(inner, captureSet)) =>
        checkSubcapture(tp1.captureSet, captureSet) && checkSubtype(tp1, inner)
      case (Type.TermArrow(params1, result1), Type.TermArrow(params2, result2)) => 
        def go(ps1: List[TermBinder], ps2: List[TermBinder])(using Context): Boolean = (ps1, ps2) match
          case (Nil, Nil) => true
          case (p1 :: ps1, p2 :: ps2) =>
            checkSubtype(p2.tpe, p1.tpe) && go(ps1, ps2)(using ctx.extend(p2))
          case _ => false
        go(params1, params2) && checkSubtype(result1, result2)(using ctx.extend(params2))
      case (Type.TypeArrow(params1, result1), Type.TypeArrow(params2, result2)) =>
        def go(ps1: List[TypeBinder | CaptureBinder], ps2: List[TypeBinder | CaptureBinder])(using Context): Boolean = (ps1, ps2) match
          case (Nil, Nil) => true
          case (CaptureBinder(_, b1) :: ps1, p2 @ CaptureBinder(_, b2) :: ps2) =>
            checkSubcapture(b2, b1) && go(ps1, ps2)(using ctx.extend(p2))
          case (TypeBinder(_, b1) :: ps1, p2 @ TypeBinder(_, b2) :: ps2) =>
            checkSubtype(b2, b1) && go(ps1, ps2)(using ctx.extend(p2))
          case _ => false
        go(params1, params2) && checkSubtype(result1, result2)(using ctx.extend(params2))
      case _ => false

