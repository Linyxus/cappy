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
      case ref => ref :: Nil
    CaptureSet(elems1).maybeWithPosFrom(captureSet)
  
