package cavia
package typechecking

import core.*
import ast.*
import reporting.trace
import expr.*

object TypeComparer:
  import TypeChecker.*
  import Expr.*
  import Binder.*
  import CaptureSet.*

  def checkSubcapture(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSubcapture(${cs1.show}, ${cs2.show})"):
    cs1.isUnsolvedUniversal ||  // If LHS is an unsolved universal, ignore it for now. 
                                // It seems to be safe because universal sets are created either covariantly or invariantly.
                                // Needs to be revisited when we have type aliases.
      cs1.elems.forall(checkSubcapture(_, cs2))

  def accountsFor(cs2: CaptureSet, x1: QualifiedRef)(using Context): Boolean =
    cs2 match
      case Const(elems) => elems.contains(x1)
      case cs2: UniversalSet if !cs2.solved =>
        val existingCs = Const(cs2.existingRefs ++ cs2.absorbedRefs)
        checkSubcapture(x1, existingCs) || {
          cs2.absorb(x1)
          true
        }
      case cs2: UniversalSet =>
        cs2.elems.contains(x1)

  def checkSubcapture(x1: QualifiedRef, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSubcapture(${x1.show}, ${cs2.show})"):
    //println(s"checkSubcapture(${x1.show}, ${cs2.show})")
    accountsFor(cs2, x1) || {
      x1.core match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) => getBinder(idx) match
          case bd: TermBinder => checkSubcapture(bd.tpe.captureSet.qualify(x1.mode), cs2)
          case bd: CaptureBinder => checkSubcapture(bd.bound.qualify(x1.mode), cs2)
          case _: TypeBinder => assert(false, "binder kind is absurd")
        case CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))) => checkSubcapture(sym.tpe.captureSet.qualify(x1.mode), cs2)
        case CaptureRef.Ref(Type.Select(base: SingletonType, fieldInfo)) =>
          def tryWiden: Boolean =
            checkSubcapture(fieldInfo.tpe.captureSet.qualify(x1.mode), cs2)
          def tryParent: Boolean =
            checkSubcapture(base.singletonCaptureSet.qualify(x1.mode), cs2)
          tryWiden || tryParent
        case CaptureRef.Selection(root, qualifier) =>
          def tryParent: Boolean =
            checkSubcapture(root.qualified(x1.mode), cs2)
          tryParent
        case _ => false
    }

  def isSameType(tp1: Type, tp2: Type)(using Context): Boolean = //trace(s"isSameType(${tp1.show}, ${tp2.show})"):
    checkSubtype(tp1, tp2) && checkSubtype(tp2, tp1)

  def checkTypeArg(arg1: Type | CaptureSet, arg2: Type | CaptureSet, variance: Variance)(using Context): Boolean =
    (arg1, arg2) match
      case (tpe1: Type, tpe2: Type) =>
        variance match
          case Variance.Covariant => checkSubtype(tpe1, tpe2)
          case Variance.Contravariant => checkSubtype(tpe2, tpe1)
          case Variance.Invariant => checkSubtype(tpe1, tpe2) && checkSubtype(tpe2, tpe1)
      case (cs1: CaptureSet, cs2: CaptureSet) =>
        variance match
          case Variance.Covariant => checkSubcapture(cs1, cs2)
          case Variance.Contravariant => checkSubcapture(cs2, cs1)
          case Variance.Invariant => checkSubcapture(cs1, cs2) && checkSubcapture(cs2, cs1)
      case _ => false

  def compareTypeArgs(args1: List[Type | CaptureSet], args2: List[Type | CaptureSet], variances: List[Variance])(using Context): Boolean =
    (args1, args2, variances) match
      case (Nil, Nil, Nil) => true
      case ((arg1: Type) :: args1, (arg2: Type) :: args2, v1 :: vs1) =>
        checkTypeArg(arg1, arg2, v1) && compareTypeArgs(args1, args2, vs1)
      case ((arg1: CaptureSet) :: args1, (arg2: CaptureSet) :: args2, v1 :: vs1) =>
        checkTypeArg(arg1, arg2, v1) && compareTypeArgs(args1, args2, vs1)
      case _ => false

  def checkSubtype(tp1: Type, tp2: Type)(using Context): Boolean = //trace(s"checkSubtype(${tp1.show}, ${tp2.show})"):
    (tp1.dealiasTypeVar, tp2.dealiasTypeVar) match
      case (_, Type.Base(BaseType.AnyType)) => true
      case (Type.Base(BaseType.NothingType), _) => true
      // case (PrimArrayType(elemType1), Type.Base(BaseType.StrType)) => 
      //   // temporary hack, in the future we should have `type String = array[char]` when type definitions are added
      //   elemType1 == Definitions.charType
      case _ if tp1 == tp2 => true
      case (Type.RefinedType(base1, refinements1), tp2) => checkSubtype(base1, tp2)
      case (tp1: Type.TypeVar, tp2: Type.TypeVar) =>
        Inference.isLessThan(tp1, tp2) || Inference.addOrder(tp1, tp2)
      case (tp1: Type.TypeVar, tp2) =>
        Inference.addBound(tp1, tp2, isUpper = true)
      case (tp1, tp2: Type.TypeVar) =>
        Inference.addBound(tp2, tp1, isUpper = false)
      case (tp1 @ Type.AppliedType(base1, args1), tp2 @ Type.AppliedType(base2, args2)) =>
        def tryDefault: Boolean =
          val variances = base1.kind match
            case TypeKind.Arrow(argVariances, _) => argVariances
            case _ => List()
          checkSubtype(base1, base2) && compareTypeArgs(args1, args2, variances)
        def tryReduceLeft: Boolean =
          val tp11 = tp1.reduce
          !(tp11 eq tp1) && checkSubtype(tp11, tp2)
        def tryReduceRight: Boolean =
          val tp22 = tp2.reduce
          !(tp22 eq tp2) && checkSubtype(tp1, tp22)
        tryDefault || tryReduceLeft || tryReduceRight
      case (tp1 @ Type.AppliedType(base1, Nil), tp2) => checkSubtype(base1, tp2)
      case (tp1, tp2 @ Type.AppliedType(base2, Nil)) => checkSubtype(tp1, base2)
      case (Type.Capturing(inner, isReadOnly1, captureSet), tp2) =>
        def modeCompatible: Boolean = !isReadOnly1 || tp2.isReadOnlyType
        modeCompatible && checkSubcapture(captureSet, tp2.captureSet) && checkSubtype(inner, tp2)
      case (tp1, Type.Capturing(inner, isReadOnly2, captureSet)) =>
        def modeCompatible: Boolean = isReadOnly2 || !tp1.isReadOnlyType
        modeCompatible && checkSubcapture(tp1.captureSet, captureSet) && checkSubtype(tp1, inner)
      case (tp1: Type.SymbolRef, tp2) if !(tp1.eval eq tp1) => checkSubtype(tp1.eval, tp2)
      case (tp1, tp2: Type.SymbolRef) if !(tp2.eval eq tp2) => checkSubtype(tp1, tp2.eval)
      case (Type.SymbolRef(sym1: StructSymbol), Type.SymbolRef(sym2: EnumSymbol)) =>
        sym2.info.variants.exists(_ eq sym1)
      case (Type.TermArrow(params1, result1), Type.TermArrow(params2, result2)) => 
        def go(ps1: List[TermBinder], ps2: List[TermBinder], checkResult: Context ?=> Boolean)(using Context): Boolean = (ps1, ps2) match
          case (Nil, Nil) => checkResult
          case (p1 :: ps1, p2 :: ps2) =>
            val (p22, _) = instantiateBinderCaps(p2)
            checkSubtype(p2.tpe, p1.tpe) && go(ps1, ps2, checkResult)(using ctx.extend(p22))
          case _ => false
        def checkResult(using Context): Boolean = checkSubtype(result1, result2)
        go(params1, params2, checkResult)
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
      case (tp1: Type.AppliedType, tp2) =>
        val tp11 = tp1.reduce
        !(tp11 eq tp1) && checkSubtype(tp11, tp2)
      case (tp1, Type.AppliedType(base2, args2)) =>
        val tp22 = tp2.reduce
        !(tp22 eq tp2) && checkSubtype(tp1, tp22)
      case (Type.Boxed(inner1), Type.Boxed(inner2)) => checkSubtype(inner1, inner2)
      case _ => false

