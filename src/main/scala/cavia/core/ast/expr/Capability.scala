package cavia
package core
package ast
package expr

import Expr.*
import Syntax.AccessMode

sealed trait CaptureSelection
case class Field(name: String) extends CaptureSelection
case class Skolem(id: Int) extends CaptureSelection

object CaptureSelection:
  private var _nextSkolemId: Int = 0

  def createSkolem(): Skolem = 
    _nextSkolemId += 1
    Skolem(_nextSkolemId)

type InstantiatedCap = CaptureRef.CapInst | CaptureRef.Selection

enum CaptureRef extends Positioned:
  case Ref(tp: SingletonType)
  case CAP()
  case CapInst(capId: Int, kind: CapKind)
  case Selection(root: CaptureRef, qualifier: CaptureSelection)

  def selectSkolem: CaptureRef =
    Selection(this, CaptureSelection.createSkolem())

  def selectField(fieldName: String): CaptureRef =
    Selection(this, Field(fieldName))

  def select(qualifier: CaptureSelection): CaptureRef =
    Selection(this, qualifier)

  def qualified(mode: Syntax.AccessMode): QualifiedRef =
    QualifiedRef(mode, this)

object CaptureRef:
  private var nextCapId: Int = 0

  def makeCapInst(kind: CapKind): CaptureRef.CapInst =
    val result: CaptureRef.CapInst = CaptureRef.CapInst(nextCapId, kind)
    nextCapId += 1
    result

  def makeSkolemCapInst(kind: CapKind, fromInst: Option[InstantiatedCap]): InstantiatedCap =
    val inst = makeCapInst(kind)
    fromInst match
      case Some(fromInst) => Selection(fromInst, CaptureSelection.createSkolem())
      case None => inst

  def makeFieldCapInst(kind: CapKind, fromInst: InstantiatedCap, fieldName: String): InstantiatedCap =
    Selection(fromInst, Field(fieldName))

  def getParents(ref: CaptureRef): Set[CaptureRef] = ref match
    case CaptureRef.Selection(root, _) =>
      getParents(root) + root
    case ref => Set(ref)

  def getRoot(ref: CaptureRef): CaptureRef = ref match
    case CaptureRef.Selection(root, _) => getRoot(root)
    case _ => ref

extension (ref: CaptureRef)
  def parents: Set[CaptureRef] = CaptureRef.getParents(ref)
  def root: CaptureRef = CaptureRef.getRoot(ref)

case class QualifiedRef(mode: Syntax.AccessMode, core: CaptureRef) extends Positioned:
  def derivedQualifiedRef(mode1: Syntax.AccessMode = mode, core1: CaptureRef = core): QualifiedRef =
    if (mode1 eq mode) && (core1 eq core) then
      this
    else
      QualifiedRef(mode1, core1).maybeWithPosFrom(this)

  def select(qualifier: CaptureSelection): QualifiedRef =
    core.select(qualifier).qualified(mode)

extension (ref: QualifiedRef)
  def isReadOnly: Boolean = ref.mode match
    case AccessMode.ReadOnly() => true
    case _ => false

  def isUniversal: Boolean = ref.core match
    case CaptureRef.CAP() => true
    case _ => false
