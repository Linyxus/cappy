package cavia
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

extension (ref: VarRef)
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
  
  override def apply(tpe: Type): Type = tpe match
    case Type.BinderRef(idx) if idx >= localBinders.size =>
      Type.BinderRef(idx - 1).like(tpe)
    case _ => mapOver(tpe)
  

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

  def showFunctionType(params: List[Binder], result: Type, cs: Option[CaptureSet] = None, isType: Boolean = false)(using ctx: TypeChecker.Context) =
    def showParams(params: List[Binder])(using ctx: TypeChecker.Context): List[String] = params match
      case Nil => Nil
      case p :: ps => 
        val s = show(p)
        s :: showParams(ps)(using ctx.extend(p))
    val paramsStr = showParams(params).mkString(", ")
    val leftBracket = if isType then "[" else "("
    val rightBracket = if isType then "]" else ")"
    val arrowStr = cs match
      case Some(cs) => s" ->${show(cs)} "
      case None => " -> "
    val resultStr = show(result)(using ctx.extend(params))
    s"${leftBracket}${paramsStr}${rightBracket}${arrowStr}${resultStr}"

  def show(tpe: Type)(using ctx: TypeChecker.Context): String = 
    //println(s"show $tpe")
    tpe match
      case Type.NoType => "<no type>"
      case Type.Base(base) => show(base)
      case Type.BinderRef(idx) => TypeChecker.getBinder(idx).name
      case Type.Capturing(inner, captureSet) => 
        inner match
          case Type.TermArrow(params, result) =>
            showFunctionType(params, result, cs = Some(captureSet), isType = false)
          case Type.TypeArrow(params, result) =>
            showFunctionType(params, result, cs = Some(captureSet), isType = true)
          case _ => s"${show(inner)}^${show(captureSet)}"
      case Type.TermArrow(params, result) => 
        showFunctionType(params, result, cs = None, isType = false)
      case Type.TypeArrow(params, result) =>
        showFunctionType(params, result, cs = None, isType = true)

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

class OpenTermBinder(tpe: Type) extends TypeMap:
  var ok: Boolean = true
  override def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
    val elems1 = captureSet.elems.flatMap: ref =>
      ref match
        case CaptureRef.Ref(Term.BinderRef(idx)) if idx >= localBinders.size => 
          if idx > localBinders.size then
            CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref) :: Nil
          else
            if variance == Variance.Covariant then
              tpe.captureSet.elems
            else if variance == Variance.Contravariant then
              Nil
            else
              ok = false
              ref :: Nil
        case _ => ref :: Nil
    CaptureSet(elems1).maybeWithPosFrom(captureSet)

  override def apply(tp: Type): Type =
    tp match
      case Type.BinderRef(idx) if idx >= localBinders.size =>
        if idx > localBinders.size then
          Type.BinderRef(idx - 1).like(tp)
        else assert(false, "openning term binder, but found it as type")
      case _ => mapOver(tp)

class OpenTermBinderExact(argRef: VarRef) extends TypeMap:
  override def mapCaptureRef(ref: CaptureRef): CaptureRef =
    ref match
      case CaptureRef.Ref(Term.BinderRef(idx)) if idx == localBinders.size =>
        argRef.asCaptureRef
      case CaptureRef.Ref(Term.BinderRef(idx)) if idx > localBinders.size =>
        CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref)
      case _ => ref

  override def apply(tp: Type): Type =
    tp match
      case Type.BinderRef(idx) if idx >= localBinders.size =>
        if idx > localBinders.size then
          Type.BinderRef(idx - 1).like(tp)
        else assert(false, "openning term binder, but found it as type")
      case _ => mapOver(tp)

