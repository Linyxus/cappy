package cavia
package typechecking

import core.*
import ast.*
import reporting.trace
import cavia.core.ast.Expr.CaptureSet.Const
import cavia.core.ast.Expr.CaptureSet.UniversalSet

object TypeComparer:
  import TypeChecker.*
  import Expr.*
  import Binder.*

  def checkSubcapture(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSubcapture(${cs1.show}, ${cs2.show})"):
    cs1.elems.forall(checkSubcapture(_, cs2))

  def accountsFor(cs2: CaptureSet, x1: CaptureRef)(using Context): Boolean =
    cs2 match
      case Const(elems) => elems.contains(x1)
      case cs2: UniversalSet if !cs2.solved =>
        val existingCs = CaptureSet.Const(cs2.existingRefs ++ cs2.absorbedRefs)
        checkSubcapture(x1, existingCs) || {
          cs2.absorb(x1)
          true
        }
      case cs2: UniversalSet =>
        cs2.elems.contains(x1)

  def checkSubcapture(x1: CaptureRef, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSubcapture(${x1.show}, ${cs2.show})"):
    //println(s"checkSubcapture(${x1.show}, ${cs2.show})")
    accountsFor(cs2, x1) || {
      x1 match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) => getBinder(idx) match
          case Binder.TermBinder(name, tpe, _) => checkSubcapture(tpe.captureSet, cs2)
          case Binder.CaptureBinder(name, bound) => checkSubcapture(bound, cs2)
          case _: Binder.TypeBinder => assert(false, "binder kind is absurd")
        case CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))) => checkSubcapture(sym.tpe.captureSet, cs2)
        case CaptureRef.Ref(Type.Select(base: SingletonType, fieldInfo)) =>
          def tryWiden: Boolean =
            checkSubcapture(fieldInfo.tpe.captureSet, cs2)
          def tryParent: Boolean =
            checkSubcapture(base.singletonCaptureSet, cs2)
          tryWiden || tryParent
        case _ => false
    }

  def isSameType(tp1: Type, tp2: Type)(using Context): Boolean = //trace(s"isSameType(${tp1.show}, ${tp2.show})"):
    checkSubtype(tp1, tp2) && checkSubtype(tp2, tp1)

  def compareTypeArgs(args1: List[Type | CaptureSet], args2: List[Type | CaptureSet])(using Context): Boolean =
    (args1, args2) match
      case (Nil, Nil) => true
      case ((arg1: Type) :: args1, (arg2: Type) :: args2) =>
        checkSubtype(arg1, arg2) && compareTypeArgs(args1, args2)
      case ((arg1: CaptureSet) :: args1, (arg2: CaptureSet) :: args2) =>
        checkSubcapture(arg1, arg2) && compareTypeArgs(args1, args2)
      case _ => false

  def checkSubtype(tp1: Type, tp2: Type)(using Context): Boolean = //trace(s"checkSubtype(${tp1.show}, ${tp2.show})"):
    (tp1.dealiasTypeVar, tp2.dealiasTypeVar) match
      case (_, Type.Base(BaseType.AnyType)) => true
      case _ if tp1 == tp2 => true
      case (Type.RefinedType(base1, refinements1), tp2) => checkSubtype(base1, tp2)
      case (tp1: Type.TypeVar, tp2: Type.TypeVar) =>
        Inference.isLessThan(tp1, tp2) || Inference.addOrder(tp1, tp2)
      case (tp1: Type.TypeVar, tp2) =>
        Inference.addBound(tp1, tp2, isUpper = true)
      case (tp1, tp2: Type.TypeVar) =>
        Inference.addBound(tp2, tp1, isUpper = false)
      case (Type.AppliedType(base1, args1), Type.AppliedType(base2, args2)) =>
        checkSubtype(base1, base2) && compareTypeArgs(args1, args2)
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
      case (Type.BinderRef(idx1), tp2) => getBinder(idx1) match
        case Binder.TypeBinder(name, bound) => checkSubtype(bound, tp2)
        case bd => assert(false, s"binder kind (idx=$idx1) is absurd, $bd")
      case _ => false

