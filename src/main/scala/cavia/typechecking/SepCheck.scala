package cavia
package typechecking

import util.boundary, boundary.break
import core.*, ast.*, expr.*, Expr.*
import TypeChecker.Context

object SepCheck:
  def derivesFrom(ref1: CaptureRef, ref2: CaptureRef): Boolean =
    ref1.parents.contains(ref2)

  def checkSeparation(ref1: QualifiedRef, ref2: QualifiedRef)(using Context): Boolean =
    def isReadOnly: Boolean = ref1.isReadOnly && ref2.isReadOnly
    def isDisjoint: Boolean =
      ref1.core != ref2.core && !derivesFrom(ref1.core, ref2.core) && !derivesFrom(ref2.core, ref1.core)
    isReadOnly || isDisjoint

  def computePeak(set: CaptureSet)(using Context): CaptureSet =
    def goRef(ref: QualifiedRef): Set[QualifiedRef] = 
      ref.core match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) =>
          TypeChecker.getBinder(idx) match
            case Binder.TermBinder(_, tpe, _) =>
              val elems = tpe.captureSet.qualify(ref.mode).elems
              goRefs(elems)
            case _: Binder.CaptureBinder => Set(ref)
            case _ => assert(false, "malformed capture set")
        case CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))) =>
          val refs = sym.tpe.captureSet.qualify(ref.mode).elems
          goRefs(refs)
        case CaptureRef.Ref(Type.Select(base, fieldInfo)) => 
          val elems = fieldInfo.tpe.captureSet.qualify(ref.mode).elems
          goRefs(elems)
        case CaptureRef.CAP() => Set(ref)
        case CaptureRef.CapInst(_, _) => Set(ref)
        case CaptureRef.Selection(root, qualifier) =>
          goRef(root.qualified(ref.mode)).map(ref => ref.select(qualifier))
    def goRefs(refs: List[QualifiedRef]): Set[QualifiedRef] =
      //println(s"goRefs ${refs.map(_.show).mkString(", ")}")
      refs.flatMap(goRef).toSet
    val elems = goRefs(set.elems)
    CaptureSet(elems.toList)

  def checkSeparation(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSeparation ${cs1.show} ${cs2.show}"):
    boundary:
      val pk1 = computePeak(cs1).elems
      val pk2 = computePeak(cs2).elems
      for i <- 0 until pk1.length do
        for j <- 0 until pk2.length do
          if !SepCheck.checkSeparation(pk1(i), pk2(j)) then
            break(false)
      true

