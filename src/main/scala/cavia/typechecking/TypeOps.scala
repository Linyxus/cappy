package cavia
package typechecking

import core.*
import ast.*
import Syntax.AccessMode
import Expr.*
import scala.collection.mutable.ArrayBuffer

class TypeMap:
  import Expr.*
  import Binder.*
  var localBinders: List[Binder] = Nil
  var variance: Variance = Variance.Covariant
  var isInsideBox: Boolean = false

  def withBinder[T](bd: Binder)(op: => T): T =
    localBinders = bd :: localBinders
    try op finally localBinders = localBinders.tail

  def withBinders[T](bds: List[Binder])(op: => T): T =
    val old = localBinders
    localBinders = bds.reverse ++ localBinders
    try op finally localBinders = old

  def withVariance[T](v: Variance)(op: => T): T =
    val old = variance
    variance = v
    try op finally variance = old

  def insideBox[T](op: => T): T =
    val old = isInsideBox
    isInsideBox = true
    try op finally isInsideBox = old

  def apply(tp: Type): Type = mapOver(tp)

  def mapBinder(param: Binder): Binder = param match
    case TermBinder(name, tpe, isConsume) => TermBinder(name, apply(tpe), isConsume).maybeWithPosFrom(param)
    case TypeBinder(name, bound) => TypeBinder(name, apply(bound)).maybeWithPosFrom(param)
    case CaptureBinder(name, bound) => CaptureBinder(name, mapCaptureSet(bound)).maybeWithPosFrom(param)

  def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
    val csets: List[CaptureSet] = captureSet.elems.map(mapCaptureRef)
    def allIdentical: Boolean = (captureSet.elems `zip` csets).forall: (ref, cset) =>
      cset.elems.size == 1 && cset.elems.head == ref
    if allIdentical then captureSet
    else csets.reduce(_ ++ _)

  def mapCaptureRef(ref: QualifiedRef): CaptureSet = ref.core match
    case CaptureRef.Ref(tp) =>
      apply(tp.asInstanceOf[Type]) match
        case tp1: SingletonType => CaptureSet(QualifiedRef(ref.mode, CaptureRef.Ref(tp1)).maybeWithPosFrom(ref) :: Nil)
        case tp1 =>
          if variance == Variance.Covariant then
            tp1.captureSet
          else if variance == Variance.Contravariant then
            CaptureSet.empty
          else 
            val cs = tp1.captureSet
            if !cs.isUnsolvedUniversal && cs.elems.size == 1 then
              cs
            else assert(false, s"Don't know how to handle a widened capture ref at invariant occurrence")
    case CaptureRef.CAP() => CaptureSet(ref :: Nil)
    case _: CaptureRef.CapInst => CaptureSet(ref :: Nil)

  def mapBaseType(base: Type.Base): Type = base

  def mapBinderRef(ref: Type.BinderRef): Type = ref

  def mapSymbolRef(ref: Type.SymbolRef): Type = ref

  def mapNoType(noType: Type.NoType): Type = noType

  def mapCapturing(ct: Type.Capturing): Type =
    ct.derivedCapturing(apply(ct.inner), ct.isReadOnly, mapCaptureSet(ct.captureSet))

  def mapTermArrow(ta: Type.TermArrow): Type =
    def go(ps: List[TermBinder], bs: List[Binder]): Type =
      ps match
        case Nil => ta.derivedTermArrow(bs.reverse, apply(ta.result))
        case p :: ps =>
          val p1 = withVariance(variance.negate):
            mapBinder(p)
          withBinder(p1):
            go(ps, p1 :: bs)
    go(ta.params, Nil)

  def mapTypeArrow(ta: Type.TypeArrow): Type =
    def go(ps: List[TypeBinder | CaptureBinder], bs: List[Binder]): Type =
      ps match
        case Nil => ta.derivedTypeArrow(bs.reverse, apply(ta.result))
        case p :: ps =>
          val p1 = withVariance(variance.negate):
            mapBinder(p)
          withBinder(p1):
            go(ps, p1 :: bs)
    go(ta.params, Nil)

  def mapAppliedType(tp: Type.AppliedType): Type =
    val tycon1 = apply(tp.constructor)
    val argVariances = tycon1.kind match
      case TypeKind.Arrow(argVariances, _) => argVariances
      case _ => List()
    assert(argVariances.length == tp.args.length)
    val args1 = tp.args.zip(argVariances).map: (arg, v) =>
      withVariance(variance * v):
        mapTypeArg(arg)
    tp.derivedAppliedType(tycon1, args1)

  def mapRefinedType(tp: Type.RefinedType): Type =
    tp.derivedRefinedType(apply(tp.base), tp.refinements.map(mapFieldInfo))

  def mapVar(tp: Type.Var): Type = tp

  def mapSelect(tp: Type.Select): Type =
    apply(tp.base.asInstanceOf[Type]) match
      case base1: SingletonType => 
        tp.derivedSelect(base1, mapFieldInfo(tp.fieldInfo))
      case baseTp => 
        //println(s"select type: $tp, baseTp = $baseTp")
        assert(false, s"Base type mapped to a non-singleton type in a select")

  def mapBoxed(tp: Type.Boxed): Type =
    insideBox:
      tp.derivedBoxed(apply(tp.core))

  def mapTypeVar(tp: Type.TypeVar): Type = tp

  def mapOver(tp: Type): Type = tp.dealiasTypeVar match
    case tp: Type.Capturing => mapCapturing(tp)
    case tp: Type.TermArrow => mapTermArrow(tp)
    case tp: Type.TypeArrow => mapTypeArrow(tp)
    case tp: Type.AppliedType => mapAppliedType(tp)
    case tp: Type.RefinedType => mapRefinedType(tp)
    case tp: Type.Base => mapBaseType(tp)
    case tp: Type.BinderRef => mapBinderRef(tp)
    case tp: Type.SymbolRef => mapSymbolRef(tp)
    case tp: Type.NoType => mapNoType(tp)
    case tp: Type.Var => mapVar(tp)
    case tp: Type.Select => mapSelect(tp)
    case tp: Type.TypeVar => mapTypeVar(tp)
    case tp: Type.Boxed => mapBoxed(tp)

  def mapFieldInfo(info: FieldInfo): FieldInfo =
    FieldInfo(info.name, apply(info.tpe), info.mutable)

  def mapTypeArg(arg: Type | CaptureSet): Type | CaptureSet = arg match
    case tpe: Type => apply(tpe)
    case cs: CaptureSet => mapCaptureSet(cs)

class ShiftType(amount: Int) extends TypeMap:
  override def mapBinderRef(ref: Type.BinderRef): Type =
    if ref.idx >= localBinders.size then
      Type.BinderRef(ref.idx + amount).like(ref)
    else
      ref

  override def mapVar(tp: Type.Var): Type = tp.ref match
    case Term.BinderRef(idx) if idx >= localBinders.size => 
      Type.Var(Term.BinderRef(idx + amount)).like(tp)
    case _ => tp
    
extension (tpe: Type)
  def shift(amount: Int): Type =
    val shift = ShiftType(amount)
    val result = shift(tpe)
    //assert(result.hasPos == tpe.hasPos && result.hasKind)
    assert(result.hasKind)
    result

extension (captureSet: CaptureSet)
  def shift(amount: Int): CaptureSet =
    val shifter = ShiftType(amount)
    val result = shifter.mapCaptureSet(captureSet)
    //assert(result.hasPos == captureSet.hasPos)
    result

extension (captureRef: QualifiedRef)
  def shift(amount: Int): QualifiedRef =
    val shifter = ShiftType(amount)
    val result = shifter.mapCaptureRef(captureRef)
    //assert(result.hasPos == captureRef.hasPos)
    assert(result.elems.size == 1)
    result.elems.head

extension (binder: Binder)
  def shift(amount: Int): Binder =
    binder match
      case Binder.TermBinder(name, tpe, isConsume) => Binder.TermBinder(name, tpe.shift(amount), isConsume).maybeWithPosFrom(binder)
      case Binder.TypeBinder(name, bound) => Binder.TypeBinder(name, bound.shift(amount)).maybeWithPosFrom(binder)
      case Binder.CaptureBinder(name, bound) => Binder.CaptureBinder(name, bound.shift(amount)).maybeWithPosFrom(binder)

extension (tpe: Type)
  def isIntegralType(using ctx: TypeChecker.Context): Boolean = tpe.dealiasTypeVar.eval(using ctx) match
    case Type.Base(base) => base.isIntegralType
    case _ => false

  def isFloatingType(using ctx: TypeChecker.Context): Boolean = tpe.dealiasTypeVar.eval(using ctx) match
    case Type.Base(base) => base.isFloatingType
    case _ => false

extension (tpe: Type)
  def captureSet: CaptureSet = tpe match
    case Type.Base(base) => CaptureSet.empty
    case Type.BinderRef(idx) => CaptureSet.empty
    case Type.SymbolRef(sym) => CaptureSet.empty
    case Type.Capturing(inner, _, captureSet) => inner.captureSet ++ captureSet
    case Type.TermArrow(params, result) => CaptureSet.empty
    case Type.TypeArrow(params, result) => CaptureSet.empty
    case Type.AppliedType(constructor, args) => CaptureSet.empty
    case Type.RefinedType(base, _) => base.captureSet
    case tp: SingletonType => tp.singletonCaptureSet
    case tvar: Type.TypeVar =>
      if tvar.instance.exists then tvar.instance.captureSet
      else CaptureSet.empty
    case Type.Boxed(core) => CaptureSet.empty
    case Type.NoType() => assert(false, "computing capture set from no type")

  def stripCaptures: Type = tpe match
    case Type.Capturing(inner, _, _) => inner.stripCaptures
    case _ => tpe

  def stripRefinements: Type = tpe match
    case Type.RefinedType(base, refinements) => base.stripRefinements
    case _ => tpe

  /** Strip capturing, refinements and boxes */
  def strip: Type = tpe match
    case Type.Capturing(inner, _, _) => inner.strip
    case Type.RefinedType(base, _) => base.strip
    case Type.Boxed(core) => core.strip
    case _ => tpe

  /** Dealias a solved type variable. */
  def dealiasTypeVar: Type = tpe match
    case Type.TypeVar(inst) if inst.exists => inst
    case _ => tpe

  def isPure(using TypeChecker.Context): Boolean =
    TypeComparer.checkSubcapture(tpe.captureSet, CaptureSet.empty)

  def isBoxedType(using TypeChecker.Context): Boolean = tpe.dealiasTypeVar match
    case Type.Boxed(core) => true
    case Type.BinderRef(idx) =>
      TypeChecker.getBinder(idx) match
        case Binder.TypeBinder(name, bound) => bound.isBoxedType
        case _ => assert(false, "(unreachable)")
    case _ => false

  def boxIfImpure(using TypeChecker.Context): Type =
    if tpe.isPure then tpe
    else Type.Boxed(tpe)

extension (t: Term)
  def asSingletonType: SingletonType = t match
    case ref: VarRef => Type.Var(ref)
    case Term.Select(base, fieldInfo) =>
      Type.Select(base.asSingletonType, fieldInfo)
    case _ => assert(false, s"Term cannot be converted to a singleton type")

  def maybeAsSingletonType: Option[SingletonType] = t match
    case ref: VarRef => Some(Type.Var(ref))
    case Term.Select(base, fieldInfo) =>
      base.maybeAsSingletonType.map(Type.Select(_, fieldInfo))
    case _ => None

extension (ref: SingletonType)
  def asCaptureRef: CaptureRef = CaptureRef.Ref(ref)
  def singletonCaptureSet: CaptureSet = CaptureSet(List(QualifiedRef(AccessMode.Normal(), asCaptureRef)))

/** A type map that approximates selections. 
 * Given p.a.type, if f(p) is not a singleton type, 
 * it looks up `a` in f(p) as the output type instead of erroring out. */
class ApproxTypeMap(using ctx: TypeChecker.Context) extends TypeMap:
  override def mapSelect(tp: Type.Select): Type =
    val base1 = apply(tp.base.asInstanceOf[Type])
    base1 match
      case base1: SingletonType => Type.Select(base1, mapFieldInfo(tp.fieldInfo))
      case base1 => 
        val ctx1 = ctx.extend(localBinders.reverse)
        //println(s"lookupinfo in $base1, tp = $tp")
        val fieldInfo1 = TypeChecker.getFieldInfo(base1, tp.fieldInfo.name)(using ctx1).get
        fieldInfo1.tpe

class AvoidLocalBinder(tpe: Type)(using ctx: TypeChecker.Context) extends ApproxTypeMap:
  var ok: Boolean = true

  // override def mapCaptureSet(captureSet: CaptureSet): CaptureSet = 
  //   val elems1 = captureSet.elems.flatMap: ref =>
  //     ref match
  //       case CapturePathOfRoot(idx) if idx == localBinders.size =>
  //         if variance == Variance.Covariant then
  //           approx.elems
  //         else if variance == Variance.Contravariant then
  //           Nil
  //         else
  //           ok = false
  //           ref :: Nil
  //       case ref @ CapturePathOfRoot(idx) if idx > localBinders.size =>
  //         CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref) :: Nil
  //       case ref => ref :: Nil
  //   CaptureSet(elems1).maybeWithPosFrom(captureSet)

  // override def apply(tpe: Type): Type = tpe match
  //   case Type.BinderRef(idx) if idx >= localBinders.size =>
  //     Type.BinderRef(idx - 1).like(tpe)
  //   case _ => mapOver(tpe)

  override def mapBinderRef(ref: Type.BinderRef): Type =
    if ref.idx >= localBinders.size then
      Type.BinderRef(ref.idx - 1).like(ref)
    else
      ref

  override def mapVar(tp: Type.Var): Type =
    tp match
      case Type.Var(Term.BinderRef(idx)) if idx > localBinders.size =>
        Type.Var(Term.BinderRef(idx - 1)).like(tp)
      case Type.Var(Term.BinderRef(idx)) if idx == localBinders.size => 
        tpe.shift(localBinders.size)
      case _ => tp

object TypePrinter:
  def show(base: BaseType): String = base match
    case BaseType.AnyType => "Any"
    case BaseType.NothingType => "Nothing"
    case BaseType.IntType => "Int"
    case BaseType.StrType => "String"
    case BaseType.UnitType => "Unit"
    case BaseType.I32 => "i32"
    case BaseType.I64 => "i64"
    case BaseType.F32 => "f32"
    case BaseType.F64 => "f64"
    case BaseType.BoolType => "bool"
    case BaseType.ArrayType => "array"
    case BaseType.CharType => "char"
    // case BaseType.BreakType => "Break"
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
      case Type.NoType() => "<no type>"
      case Type.Base(base) => show(base)
      case Type.BinderRef(idx) => TypeChecker.getBinder(idx).name
      case Type.SymbolRef(sym) => sym.name
      case tvar: Type.TypeVar =>
        if tvar.instance.exists then show(tvar.instance)
        else s"?X$$${tvar.id}"
      case Type.AppliedType(constructor, args) =>
        def showTypeArg(arg: Type | CaptureSet): String = arg match
          case tpe: Type => show(tpe)
          case cs: CaptureSet => show(cs)
        val typeArgsStr = 
          if args.isEmpty then ""
          else s"[${args.map(showTypeArg).mkString(", ")}]"
        s"${show(constructor)}$typeArgsStr"
      case Type.Capturing(inner, isReadOnly, captureSet) => 
        inner match
          case Type.TermArrow(params, result) =>
            showFunctionType(params, result, cs = Some(captureSet), isType = false)
          case Type.TypeArrow(params, result) =>
            showFunctionType(params, result, cs = Some(captureSet), isType = true)
          case _ => 
            val roStr = if isReadOnly then "ro" else ""
            s"${show(inner)}^$roStr${show(captureSet)}"
      case Type.TermArrow(params, result) => 
        showFunctionType(params, result, cs = None, isType = false)
      case Type.TypeArrow(params, result) =>
        showFunctionType(params, result, cs = None, isType = true)
      case Type.RefinedType(base, refinements) =>
        val baseStr = show(base)
        val refinementsStr = refinements.map(refinement => s"${refinement.name}: ${show(refinement.tpe)}").mkString("; ")
        s"$baseStr with { $refinementsStr }"
      case Type.Boxed(core) => s"box ${show(core)}"
      case tp: SingletonType => showSingletonType(tp) + ".type"

  def show(binder: Binder)(using TypeChecker.Context): String = binder match
    case Binder.TermBinder(name, tpe, isConsume) => 
      val consumeStr = if isConsume then "consume " else ""
      s"$consumeStr$name: ${show(tpe)}"
    case Binder.TypeBinder(name, tpe) => s"$name <: ${show(tpe)}"
    case Binder.CaptureBinder(name, tpe) => s"$name <: ${show(tpe)}"

  def show(qualifiedRef: QualifiedRef)(using TypeChecker.Context): String = qualifiedRef match
    case QualifiedRef(mode, ref) =>
      val modeStr = mode match
        case AccessMode.Normal() => ""
        case AccessMode.ReadOnly() => "ro "
        case AccessMode.Consume() => "consume "
      s"${modeStr}${show(ref)}"

  def show(captureSet: CaptureSet)(using TypeChecker.Context): String = 
    captureSet match
      case CaptureSet.Const(elems) =>
        val elemsStr = captureSet.elems.map(show).mkString(", ")
        s"{$elemsStr}"
      case cs: CaptureSet.UniversalSet => 
        val existing = cs.existingRefs.map(show).mkString(", ")
        val absorbed = cs.absorbedRefs.map(show).mkString(", ")
        s"?{${existing.mkString(", ")}} absorbing {${absorbed.mkString(", ")}}"

  def showVarRef(ref: VarRef)(using TypeChecker.Context): String = ref match
    case Term.BinderRef(idx) => TypeChecker.getBinder(idx).name
    case Term.SymbolRef(sym) => sym.name
    //case _ => assert(false, s"Not a valid VarRef: $ref")

  def showSingletonType(ref: SingletonType)(using TypeChecker.Context): String = ref match
    case Type.Var(ref) => showVarRef(ref)
    case Type.Select(base, fieldInfo) => s"${showSingletonType(base)}.${fieldInfo.name}"

  def show(captureRef: CaptureRef)(using TypeChecker.Context): String = captureRef match
    case CaptureRef.Ref(ref) => showSingletonType(ref)
    case CaptureRef.CapInst(capId, kind, fromInst) => 
      def fromText = fromInst match
        case Some(fromInst) => s"(from cap$$$fromInst)"
        case None => ""
      def capText = kind match
        case _: CapKind.Fresh => s"fresh"
        case _: CapKind.Sep => s"cap"
      s"$capText$$$capId$fromText"
    case CaptureRef.CAP() => "cap"

extension (tpe: Type)
  def show(using TypeChecker.Context): String = TypePrinter.show(tpe)

extension (cs: CaptureSet)
  def show(using TypeChecker.Context): String = TypePrinter.show(cs)

extension (ref: CaptureRef)
  def show(using TypeChecker.Context): String = TypePrinter.show(ref)

extension (cref: QualifiedRef)
  def show(using TypeChecker.Context): String = TypePrinter.show(cref)

class SubstitutionMap(args: List[Type], startingVariance: Variance = Variance.Covariant)(using TypeChecker.Context) extends ApproxTypeMap:
  var ok: Boolean = true
  variance = startingVariance

  val ctxArgs = args.reverse  // args in the order they will be in the context

  override def mapVar(tp: Type.Var): Type = tp match
    case Type.Var(Term.BinderRef(idx)) if idx >= localBinders.size =>
      val trueIdx = idx - localBinders.size
      if trueIdx < ctxArgs.size then
        ctxArgs(trueIdx).shift(localBinders.size)
      else
        Type.Var(Term.BinderRef(idx - ctxArgs.size)).like(tp)
    case _ => super.mapVar(tp)

  override def mapBinderRef(ref: Type.BinderRef): Type =
    if ref.idx >= localBinders.size then
      val trueIdx = ref.idx - localBinders.size
      assert(trueIdx >= ctxArgs.size)
      Type.BinderRef(ref.idx - ctxArgs.size).like(ref)
    else ref

// class OpenTermBinder(tpe: Type, openingIdx: Int = 0, startingVariance: Variance = Variance.Covariant) extends TypeMap:
//   var ok: Boolean = true

//   variance = startingVariance

//   override def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
//     val elems1 = captureSet.elems.flatMap: ref =>
//       ref match
//         case CaptureRef.Ref(Term.BinderRef(idx)) if idx >= localBinders.size + openingIdx => 
//           if idx > localBinders.size + openingIdx then
//             CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref) :: Nil
//           else
//             if variance == Variance.Covariant then
//               tpe.captureSet.shift(localBinders.size).elems
//             else if variance == Variance.Contravariant then
//               Nil
//             else
//               ok = false
//               ref :: Nil
//         case _ => ref :: Nil
//     CaptureSet(elems1).maybeWithPosFrom(captureSet)

//   override def apply(tp: Type): Type =
//     tp match
//       case Type.BinderRef(idx) if idx >= localBinders.size + openingIdx =>
//         if idx > localBinders.size + openingIdx then
//           Type.BinderRef(idx - 1).like(tp)
//         else assert(false, "openning term binder, but found it as type")
//       case _ => mapOver(tp)

// class OpenTermBinderExact(argRef: VarRef, openingIdx: Int = 0, startingVariance: Variance = Variance.Covariant) extends TypeMap:
//   variance = startingVariance

//   override def mapCaptureRef(ref: CaptureRef): CaptureRef =
//     ref match
//       case CaptureRef.Ref(Term.BinderRef(idx)) if idx == localBinders.size + openingIdx =>
//         argRef.asCaptureRef.shift(localBinders.size)
//       case CaptureRef.Ref(Term.BinderRef(idx)) if idx > localBinders.size + openingIdx =>
//         CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref)
//       case _ => ref

//   override def apply(tp: Type): Type =
//     tp match
//       case Type.BinderRef(idx) if idx >= localBinders.size + openingIdx =>
//         if idx > localBinders.size + openingIdx then
//           Type.BinderRef(idx - 1).like(tp)
//         else assert(false, "openning term binder, but found it as type")
//       case _ => mapOver(tp)

// class OpenCaptureBinder(argSet: CaptureSet, openingIdx: Int = 0) extends TypeMap:
//   override def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
//     val elems1 = captureSet.elems.flatMap: ref =>
//       ref match
//         case CaptureRef.Ref(Term.BinderRef(idx)) if idx >= localBinders.size + openingIdx =>
//           if idx > localBinders.size + openingIdx then
//             CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref) :: Nil
//           else
//             // println(s"Num local binders = ${localBinders.size}")
//             // println(s"openingIdx = $openingIdx")
//             // println(s"argSet = $argSet")
//             argSet.shift(localBinders.size).elems
//         case _ => ref :: Nil
//     CaptureSet(elems1).maybeWithPosFrom(captureSet)

//   override def apply(tp: Type): Type =
//     tp match
//       case Type.BinderRef(idx) if idx >= localBinders.size + openingIdx =>
//         if idx > localBinders.size + openingIdx then
//           Type.BinderRef(idx - 1).like(tp)
//         else assert(false, "opening capture binder, but found it as type")
//       case _ => mapOver(tp)

class TypeSubstitutionMap(targs: List[Type | CaptureSet], startingVariance: Variance = Variance.Covariant)(using TypeChecker.Context) extends ApproxTypeMap:
  variance = startingVariance
  val ctxArgs = targs.reverse  // args in the order they will be in the context

  override def mapCaptureRef(ref: QualifiedRef): CaptureSet = ref.core match
    case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) if idx >= localBinders.size =>
      val trueIdx = idx - localBinders.size
      if trueIdx < ctxArgs.size then
        val arg = ctxArgs(trueIdx).asInstanceOf[CaptureSet].qualify(ref.mode)
          // must be a capture set, otherwise the substitution is malformed
        arg.shift(localBinders.size)
      else
        CaptureSet(ref.derivedQualifiedRef(core1 = CaptureRef.Ref(Type.Var(Term.BinderRef(idx - ctxArgs.size)))) :: Nil)
    case _ => super.mapCaptureRef(ref)

  override def mapVar(tp: Type.Var): Type = tp match
    case Type.Var(Term.BinderRef(idx)) if idx >= localBinders.size =>
      val trueIdx = idx - localBinders.size
      assert(trueIdx >= ctxArgs.size)  // must not be a capture argument
      Type.Var(Term.BinderRef(idx - ctxArgs.size)).like(tp)
    case _ => super.mapVar(tp)

  override def mapBinderRef(ref: Type.BinderRef): Type =
    if ref.idx >= localBinders.size then
      val trueIdx = ref.idx - localBinders.size
      if trueIdx < ctxArgs.size then
        val arg = ctxArgs(trueIdx).asInstanceOf[Type]  // must be a type
        arg.shift(localBinders.size)
      else
        Type.BinderRef(ref.idx - ctxArgs.size).like(ref)
    else ref

// class OpenTypeBinder(argType: Type, openingIdx: Int = 0) extends TypeMap:
//   override def apply(tp: Type): Type =
//     tp match
//       case Type.BinderRef(idx) if idx >= localBinders.size + openingIdx =>
//         if idx > localBinders.size + openingIdx then
//           Type.BinderRef(idx - 1).like(tp)
//         else argType.shift(localBinders.size)
//       case _ => mapOver(tp)

//   override def mapCaptureRef(ref: CaptureRef): CaptureRef =
//     ref match
//       case CaptureRef.Ref(Term.BinderRef(idx)) if idx > localBinders.size + openingIdx =>
//         CaptureRef.Ref(Term.BinderRef(idx - 1)).maybeWithPosFrom(ref)
//       case CaptureRef.Ref(Term.BinderRef(idx)) if idx == localBinders.size + openingIdx =>
//         assert(false, "opening type binder, but found it as term/capture binder")
//       case _ => ref

object LazyType:
  def unapply(tpe: Type): Option[Type] = tpe match
    case Type.TypeArrow(Nil, result) => Some(result)
    case _ => None

  def apply(tpe: Type): Type = Type.TypeArrow(Nil, tpe)

def getRoot(x: SingletonType): (VarRef, VarRef => SingletonType) = x match
  case Type.Select(base, fieldInfo) => 
    val (root1, recover1) = getRoot(base)
    (root1, (base => Type.Select(recover1(base), fieldInfo)))
  case Type.Var(ref) => (ref, x => x.asSingletonType)

class CollectSignature extends TypeMap:
  val collected: ArrayBuffer[QualifiedRef] = ArrayBuffer.empty

  override def mapCaptureRef(ref: QualifiedRef): CaptureSet = 
    ref.core match
      // case CaptureRef.Ref(Term.BinderRef(idx)) if idx < localBinders.size =>
      // case CaptureRef.Ref(Term.BinderRef(idx)) if idx >= localBinders.size =>
      //   collected += CaptureRef.Ref(Term.BinderRef(idx - localBinders.size))
      case CaptureRef.Ref(path) =>
        val (root, _) = getRoot(path)
        root match
          case Term.BinderRef(idx) if idx < localBinders.size =>
          case root => collected += ref
      case CaptureRef.CAP() => // do not collect general caps
      case _ => collected += ref
    CaptureSet(ref :: Nil)

  override def apply(tpe: Type): Type = tpe match
    case Type.TypeArrow(ps, result) =>
      withBinders(ps):
        apply(result)
    case Type.TermArrow(ps, result) =>
      def go(ps: List[Binder.TermBinder]): Unit = ps match
        case Nil => ()
        case p :: ps =>
          mapCaptureSet(p.tpe.captureSet)
          withBinder(p):
            go(ps)
      go(ps)
      withBinders(ps):
        apply(result)
    case _ => mapOver(tpe)

extension (tpe: Type)
  def signatureCaptureSet: CaptureSet =
    val collector = CollectSignature()
    collector(tpe)
    CaptureSet(collector.collected.toList)

class CapInstantiation(createCapInst: () => CaptureRef.CapInst) extends TypeMap:
  var localCaps: List[CaptureRef.CapInst] = Nil

  override def apply(tpe: Type): Type = tpe match
    case Type.TypeArrow(ps, result) => tpe
    case Type.TermArrow(ps, result) => tpe
    case _ => mapOver(tpe)

  override def mapCaptureRef(ref: QualifiedRef): CaptureSet = 
    if variance == Variance.Contravariant then
      CaptureSet(ref :: Nil)  // do nothing at contravariant sites
    else
      ref.core match
        case CaptureRef.CAP() => 
          val inst: CaptureRef.CapInst = createCapInst()
          localCaps = inst :: localCaps
          CaptureSet(QualifiedRef(ref.mode, inst) :: Nil)
        case _ => CaptureSet(ref :: Nil)

class UniversalConversion extends TypeMap:
  import scala.collection.mutable.Set
  var createdUniversals: List[CaptureSet.UniversalSet] = Nil

  def maybeCreateUniversal(elems: List[QualifiedRef]): CaptureSet =
    if elems.exists(_.isUniversal) then
      val existingRefs = elems.filterNot(_.isUniversal)
      val univSet = CaptureSet.UniversalSet(existingRefs)
      createdUniversals = univSet :: createdUniversals
      univSet
    else CaptureSet.Const(elems)

  override def mapCaptureSet(captureSet: CaptureSet): CaptureSet = 
    if variance == Variance.Contravariant then
      captureSet  // do nothing
    else
      captureSet match
        case CaptureSet.Const(elems) => maybeCreateUniversal(elems)
        case univ: CaptureSet.UniversalSet if univ.solved => maybeCreateUniversal(univ.elems)
        case univ: CaptureSet.UniversalSet =>
          assert(false, "universal conversion on a non-solved universal set")

  override def apply(tpe: Type): Type = tpe match
    case Type.TypeArrow(ps, result) => tpe
    case Type.TermArrow(ps, result) => tpe
    case _ => mapOver(tpe)

object PrimArrayType:
  def unapply(tpe: Type): Option[Type] = tpe match
    case Type.AppliedType(Type.Base(BaseType.ArrayType), (elemType: Type) :: Nil) => Some(elemType)
    case _ => None

// object BreakCapabilityType:
//   def unapply(tpe: Type): Option[Type] = tpe match
//     case Type.AppliedType(Type.Base(BaseType.BreakType), (returnType: Type) :: Nil) => Some(returnType)
//     case _ => None

object AppliedStructType:
  def unapply(tpe: Type): Option[(StructSymbol, List[Type | CaptureSet])] = tpe match
    case Type.SymbolRef(sym: StructSymbol) => Some((sym, Nil))
    case Type.AppliedType(Type.SymbolRef(sym: StructSymbol), typeArgs) => Some((sym, typeArgs))
    case _ => None

object AppliedEnumType:
  def unapply(tpe: Type): Option[(EnumSymbol, List[Type | CaptureSet])] = tpe match
    case Type.SymbolRef(sym: EnumSymbol) => Some((sym, Nil))
    case Type.AppliedType(Type.SymbolRef(sym: EnumSymbol), typeArgs) => Some((sym, typeArgs))
    case _ => None

  def apply(enumSym: EnumSymbol, typeArgs: List[Type | CaptureSet]): Type =
    Type.AppliedType(Type.SymbolRef(enumSym), typeArgs)

extension (tpe: Type)
  def refined(refinements: List[FieldInfo]): Type = 
    if refinements.isEmpty then tpe
    else tpe match
      case Type.Capturing(base, isReadOnly, captureSet) =>
        tpe.derivedCapturing(base.refined(refinements), isReadOnly, captureSet)
      case tpe => tpe.derivedRefinedType(tpe, refinements)
  
  def isReadOnly: Boolean = tpe match
    case Type.Capturing(_, isReadOnly, _) => isReadOnly
    case Type.RefinedType(base, _) => base.isReadOnly
    case _ => false

  def asReadOnly: Type = tpe match
    case Type.Capturing(base, _, captureSet) => Type.Capturing(base.asReadOnly, isReadOnly = true, captureSet)
    case Type.RefinedType(base, refinements) => Type.RefinedType(base.asReadOnly, refinements)
    case tpe => tpe

extension (ref: QualifiedRef)
  def isReadOnly: Boolean = ref.mode match
    case AccessMode.ReadOnly() => true
    case _ => false

  def isUniversal: Boolean = ref.core match
    case CaptureRef.CAP() => true
    case _ => false

object CheckVariance:
  case class Mismatch(idx: Int, useSite: Variance)

def checkVarianceUsage(use: Variance, defined: Variance): Boolean =
  (use, defined) match
    case (Variance.Covariant, Variance.Covariant) => true
    case (Variance.Contravariant, Variance.Contravariant) => true
    case (_, Variance.Invariant) => true
    case _ => false

class CheckVariance(binderVariances: List[Variance], startingVariance: Variance = Variance.Covariant) extends TypeMap:
  import CheckVariance.*
  var mismatches: List[Mismatch] = Nil
  variance = startingVariance

  override def mapBinderRef(ref: Type.BinderRef): Type =
    if ref.idx >= localBinders.size then
      val globalIdx = ref.idx - localBinders.size
      val definedVariance = binderVariances(globalIdx)
      if !checkVarianceUsage(variance, definedVariance) then
        mismatches = Mismatch(globalIdx, variance) :: mismatches
    ref

def showVariance(v: Variance): String = v match
  case Variance.Covariant => "covariant"
  case Variance.Contravariant => "contravariant"
  case Variance.Invariant => "invariant"

extension (tpe: Type)
  def reduce(using ctx: TypeChecker.Context): Type = tpe match
    case Type.Capturing(base, isReadOnly, captureSet) =>
      tpe.derivedCapturing(base.reduce, isReadOnly, captureSet)
    case Type.RefinedType(base, refinements) =>
      tpe.derivedRefinedType(base.reduce, refinements)
    case Type.SymbolRef(sym: TypeDefSymbol) if sym.info.typeParams.isEmpty =>
      sym.info.body
    case Type.AppliedType(Type.SymbolRef(baseSym: TypeDefSymbol), args) => 
      val body = baseSym.info.body
      val body1 = TypeChecker.substituteType(body, args, isParamType = false)
      body1
    case _ => tpe

  def eval(using ctx: TypeChecker.Context): Type =
    val tpe1 = tpe.reduce
    if tpe1 eq tpe then tpe
    else tpe1.eval

extension (tpe: Type)
  def simplify(using ctx: TypeChecker.Context): Type = 
    val tpe1 =  tpe.stripCaptures.dealiasTypeVar.eval
    if tpe1 eq tpe then tpe
    else tpe1.simplify
