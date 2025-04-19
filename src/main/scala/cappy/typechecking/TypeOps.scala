package cappy
package typechecking

import core.*
import ast.*
import Expr.*

extension (tpe: Type)
  def captureSet: CaptureSet = tpe match
    case Type.Base(base) => CaptureSet.empty
    case Type.BinderRef(idx) => CaptureSet.empty
    case Type.Capturing(inner, captureSet) => inner.captureSet ++ captureSet
    case Type.TermArrow(params, result) => CaptureSet.empty
    case Type.TypeArrow(params, result) => CaptureSet.empty
    case Type.NoType => assert(false, "computing capture set from no type")

  def stripCaptures: Type = tpe match
    case Type.Capturing(inner, captureSet) => inner.stripCaptures
    case _ => tpe

  def isPure(using TypeChecker.Context): Boolean =
    TypeComparer.checkSubcapture(tpe.captureSet, CaptureSet.empty)

extension (ref: Term.BinderRef)
  def asCaptureRef: CaptureRef = CaptureRef.Ref(ref).maybeWithPosFrom(ref)
  def singletonCaptureSet: CaptureSet = CaptureSet(List(ref.asCaptureRef))

extension (ref: Term.SymbolRef)
  def asCaptureRef: CaptureRef = CaptureRef.Ref(ref).maybeWithPosFrom(ref)
  def singletonCaptureSet: CaptureSet = CaptureSet(List(ref.asCaptureRef))

class AvoidLocalBinder(approx: CaptureSet) extends TypeMap:
  var ok: Boolean = true
  override def mapCaptureSet(captureSet: CaptureSet): CaptureSet = 
    val elems1 = captureSet.elems.flatMap:
      case ref @ CaptureRef.Ref(Term.BinderRef(idx)) if idx == localBinders.size =>
        if variance == Variance.Covariant then
          approx.elems
        else if variance == Variance.Contravariant then
          Nil
        else
          ok = false
          ref :: Nil
      case ref @ CaptureRef.Ref(Term.BinderRef(idx)) if idx > localBinders.size =>
        CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref) :: Nil
      case ref => ref :: Nil
    CaptureSet(elems1).maybeWithPosFrom(captureSet)

object TypePrinter:
  def show(base: BaseType): String = base match
    case BaseType.AnyType => "Any"
    case BaseType.IntType => "Int"
    case BaseType.StrType => "Str"
    case BaseType.UnitType => "Unit"
    case BaseType.I32 => "i32"
    case BaseType.I64 => "i64"
    case BaseType.F32 => "f32"
    case BaseType.F64 => "f64"

  def show(tpe: Type)(using ctx: TypeChecker.Context): String = 
    //println(s"show $tpe")
    tpe match
      case Type.NoType => "<no type>"
      case Type.Base(base) => show(base)
      case Type.BinderRef(idx) => TypeChecker.getBinder(idx).name
      case Type.Capturing(inner, captureSet) => s"${show(inner)}^${show(captureSet)}"
      case Type.TermArrow(params, result) => 
        def showParams(params: List[Binder.TermBinder])(using TypeChecker.Context): List[String] = params match
          case Nil => Nil
          case p :: ps => 
            val s = show(p)
            s :: showParams(ps)(using ctx.extend(p))
        s"(${showParams(params).mkString(", ")}) -> ${show(result)(using ctx.extend(params))}"
      case Type.TypeArrow(params, result) =>
        def showParams(params: List[Binder.TypeBinder | Binder.CaptureBinder])(using TypeChecker.Context): List[String] = params match
          case Nil => Nil
          case p :: ps => 
            val s = show(p)
            s :: showParams(ps)(using ctx.extend(p))
        s"[${showParams(params).mkString(", ")}] -> ${show(result)(using ctx.extend(params))}"

  def show(binder: Binder)(using TypeChecker.Context): String = binder match
    case Binder.TermBinder(name, tpe) => s"$name: ${show(tpe)}"
    case Binder.TypeBinder(name, tpe) => s"$name <: ${show(tpe)}"
    case Binder.CaptureBinder(name, tpe) => s"$name <: ${show(tpe)}"

  def show(captureSet: CaptureSet)(using TypeChecker.Context): String = 
    val elems = captureSet.elems.map(show).mkString(", ")
    s"{$elems}"

  def show(captureRef: CaptureRef)(using TypeChecker.Context): String = captureRef match
    case CaptureRef.Ref(Term.BinderRef(idx)) => TypeChecker.getBinder(idx).name
    case CaptureRef.Ref(Term.SymbolRef(sym)) => sym.name
    case CaptureRef.CAP() => "cap"

extension (tpe: Type)
  def show(using TypeChecker.Context): String = TypePrinter.show(tpe)
  
