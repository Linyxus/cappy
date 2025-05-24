package cavia
package typechecking

import scala.util.boundary, boundary.break
import scala.collection.mutable
import core.*
import ast.*

import Expr.*

object Inference:
  import TypeChecker.*

  /** State for type inference. */
  case class InferenceState(
    // A directory of local type variables
    localVars: mutable.ArrayBuffer[TypeVarInfo],
  ):
    /** An inference state in a new scope. Copies all fields except setting localVars to empty. */
    def derivedState: InferenceState =
      copy(
        localVars = mutable.ArrayBuffer.empty,
      )

  object InferenceState:
    def empty: InferenceState = InferenceState(
      localVars = mutable.ArrayBuffer.empty,
    )

  def state(using Context): InferenceState = ctx.inferenceState

  def createTypeVar(paramName: String, srcPos: SourcePos, upperBound: Type = Type.NoType())(using Context): Type.TypeVar =
    val tvarInfo = TypeVarInfo(
      name = "?" + paramName,
      level = ctx.binders.size,
      lowerBound = Type.NoType(),
      upperBound = upperBound,
      instance = Type.NoType(),
      upperDeps = mutable.Set.empty,
      lowerDeps = mutable.Set.empty,
    )
    state.localVars += tvarInfo
    tvarInfo.setProvenance:
      TypeVarProvenance(paramName, srcPos)
    if upperBound.exists then
      tvarInfo.addBound(upperBound, isUpper = true)
    Type.TypeVar(tvarInfo)

  def isLessThan(tv1: Type.TypeVar, tv2: Type.TypeVar)(using Context): Boolean =
    tv1.info.upperDeps.contains(tv2.info)

  def addOrder(tv1: Type.TypeVar, tv2: Type.TypeVar)(using Context): Boolean =
    tv1.info.addDep(tv2.info, isUpper = true) &&
      tv2.info.addDep(tv1.info, isUpper = false)

  def addBound(tv: Type.TypeVar, bound: Type, isUpper: Boolean)(using Context): Boolean =
    tv.info.addBound(bound, isUpper)

  def solveTypeVars()(using Context): Unit =
    state.localVars.foreach: tv =>
      tv.solve()
