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

  def stripCaptures: Type = tpe match
    case Type.Base(base) => tpe
    case Type.BinderRef(idx) => tpe
    case Type.Capturing(inner, captureSet) => inner.stripCaptures
    case Type.TermArrow(params, result) => Type.TermArrow(params, result.stripCaptures)
    case Type.TypeArrow(params, result) => Type.TypeArrow(params, result.stripCaptures)

extension (ref: Term.BinderRef)
  def singletonCaptureSet: CaptureSet = CaptureSet(List(CaptureRef.BinderRef(ref.idx)))

class AvoidLocalBinder(approx: CaptureSet) extends TypeMap:
  var ok: Boolean = true
  override def mapCaptureSet(captureSet: CaptureSet): CaptureSet = 
    val elems1 = captureSet.elems.flatMap:
      case ref @ CaptureRef.BinderRef(idx) if idx == localBinders.size =>
        if variance == Variance.Covariant then
          approx.elems
        else if variance == Variance.Contravariant then
          Nil
        else
          ok = false
          ref :: Nil
      case ref @ CaptureRef.BinderRef(idx) if idx > localBinders.size =>
        CaptureRef.BinderRef(idx - 1).maybeWithPosFrom(ref) :: Nil
      case ref => ref :: Nil
    CaptureSet(elems1).maybeWithPosFrom(captureSet)

object TypePrinter:
  def show(base: BaseType): String = base match
    case BaseType.AnyType => "Any"
    case BaseType.IntType => "Int"
    case BaseType.StrType => "Str"
    case BaseType.UnitType => "Unit"

  def show(tpe: Type)(using TypeChecker.Context): String = tpe match
    case Type.Base(base) => show(base)
    case Type.BinderRef(idx) => TypeChecker.getBinder(idx).name
    case Type.Capturing(inner, captureSet) => s"${show(inner)}^{${show(captureSet)}}"
    case Type.TermArrow(params, result) => s"(${params.map(show).mkString(", ")}) -> ${show(result)}"
    case Type.TypeArrow(params, result) => s"(${params.map(show).mkString(", ")}) -> ${show(result)}"

  def show(binder: Binder)(using TypeChecker.Context): String = binder match
    case Binder.TermBinder(name, tpe) => s"$name: ${show(tpe)}"
    case Binder.TypeBinder(name, tpe) => s"$name: ${show(tpe)}"
    case Binder.CaptureBinder(name, tpe) => s"$name: ${show(tpe)}"

  def show(captureSet: CaptureSet)(using TypeChecker.Context): String = 
    captureSet.elems.map(show).mkString(", ")

  def show(captureRef: CaptureRef)(using TypeChecker.Context): String = captureRef match
    case CaptureRef.BinderRef(idx) => TypeChecker.getBinder(idx).name
    case CaptureRef.CAP() => "cap"

extension (tpe: Type)
  def show(using TypeChecker.Context): String = TypePrinter.show(tpe)
  
