package cavia
package typechecking

import scala.util.boundary, boundary.break
import scala.collection.mutable
import core.*
import ast.*

import Expr.*

object Inference:
  import TypeChecker.*

  case class InferenceState(
    // A directory of local type variables
    localVars: mutable.ArrayBuffer[Type.TypeVar],
    // Lower bounds of type variables
    lowerBounds: mutable.Map[Int, Type],
    // Upper bounds of type variables
    upperBounds: mutable.Map[Int, Type],
    // Dependencies between type variables
    uppers: mutable.Map[Int, Set[Type.TypeVar]],
    lowers: mutable.Map[Int, Set[Type.TypeVar]],
  )

  object InferenceState:
    def empty: InferenceState = InferenceState(
      localVars = mutable.ArrayBuffer.empty,
      lowerBounds = mutable.Map.empty,
      upperBounds = mutable.Map.empty,
      uppers = mutable.Map.empty,
      lowers = mutable.Map.empty,
    )

  def state(using Context): InferenceState = ctx.inferenceState

  def createTypeVar(upperBound: Type = Type.NoType())(using Context): Type.TypeVar =
    val tvar: Type.TypeVar = Type.TypeVar()
    ctx.inferenceState.localVars += tvar
    if upperBound.exists then
      recordBound(tvar, upperBound, isUpper = true)
    tvar

  def recordBound(tvar: Type.TypeVar, bound: Type, isUpper: Boolean)(using Context): Boolean = boundary:
    assert(state.localVars.contains(tvar), s"Type variable ${tvar} is not a local variable")
    if isUpper then
      def lowerOk: Boolean =
        val lb = lowerBoundOf(tvar)
        !lb.exists || TypeComparer.checkSubtype(lb, bound)
      def newUpper: Type =
        val ub = upperBoundOf(tvar)
        if !ub.exists then bound
        else if TypeComparer.checkSubtype(bound, ub) then bound
        else if TypeComparer.checkSubtype(ub, bound) then ub
        else break(false)
      lowerOk && {
        state.upperBounds(tvar.id) = newUpper
        true
      }
    else
      def upperOk: Boolean =
        val ub = upperBoundOf(tvar)
        !ub.exists || TypeComparer.checkSubtype(bound, ub)
      val newLower: Type =
        val lb = lowerBoundOf(tvar)
        if !lb.exists then bound
        else if TypeComparer.checkSubtype(lb, bound) then bound
        else if TypeComparer.checkSubtype(bound, lb) then lb
        else break(false)
      upperOk && {
        state.lowerBounds(tvar.id) = newLower
        true
      }

  def addBound(tvar: Type.TypeVar, bound: Type, isUpper: Boolean)(using Context): Boolean =
    recordBound(tvar, bound, isUpper) && {
      if isUpper then
        lowersOf(tvar).forall(tv => recordBound(tv, bound, isUpper = true))
      else
        uppersOf(tvar).forall(tv => recordBound(tv, bound, isUpper = false))
    }

  /** Record an order that tv1 <: tv2 */
  def recordOrder(tv1: Type.TypeVar, tv2: Type.TypeVar)(using Context): Unit =
    if tv1.id != tv2.id then
      state.uppers(tv1.id) = uppersOf(tv1) + tv2
      state.lowers(tv2.id) = lowersOf(tv2) + tv1

  /** Add an order that tv1 <: tv2 and propagate all constraints */
  def addOrder(tv1: Type.TypeVar, tv2: Type.TypeVar)(using Context): Boolean =
    if tv1.id == tv2.id then true
    else 
      val lowers = tv1 :: lowersOf(tv1).toList
      val uppers = tv2 :: uppersOf(tv2).toList
      val lowerBounds = lowers.map(lowerBoundOf).filter(_.exists)
      val upperBounds = uppers.map(upperBoundOf).filter(_.exists)
      
      def recordOrders: Boolean =
        for l <- lowers do
          for u <- uppers do
            recordOrder(l, u)
        true
      
      def recordBounds: Boolean =
        def lowerOk: Boolean =
          lowerBounds.forall(lb => uppers.forall(u => recordBound(u, lb, isUpper = false)))
        def upperOk: Boolean =
          upperBounds.forall(ub => lowers.forall(l => recordBound(l, ub, isUpper = true)))
        lowerOk && upperOk

      recordOrders && recordBounds

  def isLessThan(tv1: Type.TypeVar, tv2: Type.TypeVar)(using Context): Boolean =
    tv1.id == tv2.id || uppersOf(tv1).contains(tv2)

  def uppersOf(tvar: Type.TypeVar)(using Context): Set[Type.TypeVar] =
    ctx.inferenceState.uppers.getOrElse(tvar.id, Set.empty)

  def lowersOf(tvar: Type.TypeVar)(using Context): Set[Type.TypeVar] =
    ctx.inferenceState.lowers.getOrElse(tvar.id, Set.empty)

  def lowerBoundOf(tvar: Type.TypeVar)(using Context): Type =
    ctx.inferenceState.lowerBounds.getOrElse(tvar.id, Type.NoType())

  def upperBoundOf(tvar: Type.TypeVar)(using Context): Type =
    ctx.inferenceState.upperBounds.getOrElse(tvar.id, Type.NoType())

  def solveTypeVars()(using Context): Unit =
    state.localVars.foreach: tv =>
      val lb = lowerBoundOf(tv)
      val ub = upperBoundOf(tv)
      val inst =
        if lb.exists then lb
        else if ub.exists then ub
        else Definitions.nothingType
      tv.instance = inst
