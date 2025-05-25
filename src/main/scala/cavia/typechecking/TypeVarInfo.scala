package cavia
package typechecking

import util.boundary, boundary.break
import scala.collection.mutable

import core.*
import ast.*, expr.*
import Expr.*
import TypeChecker.*

case class TypeVarInfo(
  name: String,
  level: Int,
  var lowerBound: Type,
  var upperBound: Type,
  var instance: Type,
  val upperDeps: mutable.Set[TypeVarInfo],
  val lowerDeps: mutable.Set[TypeVarInfo],
) extends HasProvenance:
  def isSolved: Boolean = instance.exists

  /** Add this bound to the type variable.
   * @pre `bound` is not a type variable.
   */
  def addThisBound(bound: Type, isUpper: Boolean)(using Context): Boolean = boundary:
    assert(!isSolved)
    val curLevel = ctx.binders.size
    val bound1 =
      if curLevel > this.level then
        unshiftTypeByN(bound, curLevel - this.level) match
          case None => break(false)
          case Some(tpe) => tpe
      else bound
    if isUpper then
      def lowerOk: Boolean =
        val lb = lowerBound
        !lb.exists || TypeComparer.checkSubtype(lb, bound1)
      def newUpper: Type =
        val ub = upperBound
        if !ub.exists then bound1
        else if TypeComparer.checkSubtype(bound1, ub) then bound1
        else if TypeComparer.checkSubtype(ub, bound1) then ub
        else break(false)
      lowerOk && {
        upperBound = newUpper
        true
      }
    else
      def upperOk: Boolean =
        val ub = upperBound
        !ub.exists || TypeComparer.checkSubtype(bound1, ub)
      val newLower: Type =
        val lb = lowerBound
        if !lb.exists then bound1
        else if TypeComparer.checkSubtype(lb, bound1) then bound1
        else if TypeComparer.checkSubtype(bound1, lb) then lb
        else break(false)
      upperOk && {
        lowerBound = newLower
        true
      }

  def subsumesBound(bound: Type, isUpper: Boolean)(using Context): Boolean = boundary:
    if isUpper then
      val ub = upperBound
      ub.exists && TypeComparer.checkSubtype(ub, bound)
    else
      val lb = lowerBound
      lb.exists && TypeComparer.checkSubtype(bound, lb)

  def addBound(bound: Type, isUpper: Boolean)(using Context): Boolean = boundary:
    subsumesBound(bound, isUpper) || addThisBound(bound, isUpper) && {
      // Propagate bounds transitively.
      if isUpper then
        lowerDeps.forall(_.addBound(bound, isUpper = true))
      else
        upperDeps.forall(_.addBound(bound, isUpper = false))
    }

  def addThisDep(other: TypeVarInfo, isUpper: Boolean)(using Context): Boolean =
    assert(!isSolved && !(this eq other))
    this.level == other.level && {
      if isUpper then
        upperDeps += other
        other.lowerDeps += this
        (!lowerBound.exists || other.addBound(lowerBound, isUpper = false)) &&
          (!other.upperBound.exists || addBound(other.upperBound, isUpper = true))
      else
        lowerDeps += other
        other.upperDeps += this
        (!upperBound.exists || other.addBound(upperBound, isUpper = true)) &&
          (!other.lowerBound.exists || addBound(other.lowerBound, isUpper = false))
    }

  def addDep(other: TypeVarInfo, isUpper: Boolean)(using Context): Boolean = boundary:
    assert(!isSolved)
    if this eq other then true
    else if isUpper && upperDeps.contains(other) then true
    else if !isUpper && lowerDeps.contains(other) then true
    else addThisDep(other, isUpper)

  def solve()(using Context): Unit = boundary:
    if isSolved then break(())
    val lb = lowerBound
    val ub = upperBound
    val inst =
      if lb.exists then lb
      else if ub.exists then ub
      else Definitions.nothingType
    this.instance = inst
