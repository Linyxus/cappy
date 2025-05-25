package cavia
package typechecking

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
import core.*
import ast.*
import reporting.*
import expr.*

/** Type checker. */
object TypeChecker:
  import Syntax.AccessMode
  import Expr.*
  import Binder.*
  import Inference.*

  val PeakConsumed = new MetaKey:
    type Value = Set[CaptureRef.CapInst]

  val PeakCreated = new MetaKey:
    type Value = Set[CaptureRef.CapInst]

  /** Type checking context. */
  case class Context(
    /** Binders in scope. */
    binders: List[Binder], 
    /** Symbols in scope. */
    symbols: List[Symbol], 
    /** State for type inference. */
    inferenceState: InferenceState, 
    /** Capabilities that have been consumed. */
    consumedPeaks: CaptureSet = CaptureSet.empty, 
    /** Level of freshness. */
    freshLevel: Int = 0,
    /** Deprecated: `return` has been dropped. */
    // defReturnType: Option[Type] = None,
  ):
    /** Extend the context with a list of binders. */
    def extend(bds: List[Binder]): Context =
      if bds.isEmpty then this
      else
        var newBinders = binders
        for bd <- bds do
          newBinders = bd :: newBinders
        copy(binders = newBinders)
    def extend(bd: Binder): Context = extend(bd :: Nil)

    /** Add a symbol to the context. */
    def addSymbol(sym: Symbol): Context =
      copy(symbols = sym :: symbols)

    /** Add a list of symbols to the context. */
    def addSymbols(syms: List[Symbol]): Context =
      if syms.isEmpty then this
      else
        var newSymbols = symbols
        for sym <- syms do
          newSymbols = sym :: newSymbols
        copy(symbols = newSymbols)

    /** Create a new scope in type inference. See [[InferenceState]]. */
    def newInferenceScope: Context =
      copy(inferenceState = this.inferenceState.derivedState)

    /** Enter a new fresh level.
     * This happens when we are entering a new closure.
     */
    def newFreshLevel: Context =
      copy(freshLevel = freshLevel + 1)

    /** Add a set of consumed peaks to the context. */
    def moreConsumedPeaks(peaks: CaptureSet): Context =
      copy(consumedPeaks = consumedPeaks ++ peaks)

    // def withDefReturnType(tpe: Type): Context =
    //   copy(defReturnType = Some(tpe))

  object Context:
    def empty: Context = 
      Context(
        Nil, 
        Nil, 
        InferenceState.empty, 
        consumedPeaks = CaptureSet.empty, 
        freshLevel = 0
      )


  enum TypeError extends Positioned:
    case UnboundVariable(name: String, addenda: String = "")
    case TypeMismatch(expected: String, actual: String)
    case LeakingLocalBinder(tp: String)
    case SeparationError(cs1: EntityWithProvenance, cs2: EntityWithProvenance)
    case ConsumedError(cs: String, consumedPos: Option[SourcePos])
    case GeneralError(msg: String)
    case ImpureInferredType(tpeStr: String)

    def show: String = this match
      case UnboundVariable(name, addenda) => 
        val addendaStr = if addenda.isEmpty then "" else s" ($addenda)"
        s"ERROR: Cannot find the identifier $name$addendaStr"
      case TypeMismatch(expected, actual) => s"ERROR: Type mismatch, expected $expected, but got $actual"
      case LeakingLocalBinder(tp) => s"ERROR: Leaking local binder: $tp"
      case SeparationError(cs1, cs2) => s"ERROR: Separation error, $cs1 and $cs2 are not separated"
      case GeneralError(msg) => msg
      case ConsumedError(cs, consumedPos) => s"ERROR: This uses a consumed capability: $cs"
      case ImpureInferredType(tpeStr) => s"ERROR: The inferred type argument $tpeStr is impure. This is inferred from the term arguments. Consider box them."

    def asMessage: Message = this match
      case SeparationError(entity1, entity2) =>
        val parts = List(
          MessagePart(List(s"ERROR(separation): ${entity1.entity} and ${entity2.entity} are not separated, where"), this.pos),
          MessagePart(List(s"... ${entity1.entity} comes from ${entity1.provenance}"), entity1.provenancePos),
          MessagePart(List(s"... and ${entity2.entity} comes from ${entity2.provenance}"), entity2.provenancePos),
        )
        Message(parts)
      case ConsumedError(cs, consumedPos) =>
        val parts = List(
          MessagePart(List(s"ERROR(consume): This uses a consumed capability $cs"), pos),
        ) ++ consumedPos.map: pos =>
          MessagePart(List("... which was consumed here"), pos)
        Message(parts)
      case _ => Message.simple(show, pos)

  def ctx(using myCtx: Context): Context = myCtx

  def withinNewInferenceScope[T](body: Context ?=> T)(using Context): T =
    body(using ctx.newInferenceScope)
  
  type Result[+A] = Either[List[TypeError], A]

  def lookupBinder(name: String)(using ctx: Context): Option[(Binder, Int)] =
    ctx.binders.zipWithIndex.find((binder, _) => binder.name == name).map: (bd, idx) =>
      (bd.shift(idx + 1), idx)

  def findPrimitiveSymbol(name: String)(using ctx: Context): Option[Symbol] =
    if name == "WASM_MEMORY" then
      Some(Definitions.MemorySymbol)
    else
      None

  def findDefinedSymbol(name: String)(using ctx: Context): Option[Symbol] = boundary:
    for sym <- ctx.symbols do
      if sym.name == name then
        break(Some(sym))
      sym match
        case sym: EnumSymbol =>
          sym.info.variants.find(_.name == name) match
            case Some(variant) => break(Some(variant))
            case None => ()
        case _ => ()
    None

  def lookupSymbol(name: String)(using ctx: Context): Option[Symbol] =
    findPrimitiveSymbol(name) `orElse` findDefinedSymbol(name)

  def lookupAll(name: String)(using ctx: Context): Option[(Binder, Int) | Symbol] =
    lookupBinder(name) match
      case Some(bd) => Some(bd)
      case None => lookupSymbol(name) match
        case Some(sym) => Some(sym)
        case None => None

  def getBinder(idx: Int)(using ctx: Context): Binder = //trace(s"getBinder $idx"):
    assert(idx >= 0 && idx < ctx.binders.length, s"invalid binder index: $idx")
    val bd = ctx.binders(idx)
    val res = bd.shift(idx + 1)
    res

  def checkTermParam(param: Syntax.TermParam, pt: Type = Type.NoType())(using ctx: Context): Result[TermBinder] =
    hopefully:
      param.tpe match
        case Some(tpe) =>
          val tpe1 = checkType(tpe).!!
          val binder: TermBinder = TermBinder(param.name, tpe1, param.isConsume)
          binder.maybeWithPosFrom(param)
        case None if pt.exists =>
          val binder: TermBinder = TermBinder(param.name, pt, param.isConsume)
          binder.maybeWithPosFrom(param)
        case None =>
          sorry(TypeError.GeneralError("Cannot infer type for lambda parameter").withPos(param.pos))

  def checkTypeParam(param: Syntax.TypeParam)(using ctx: Context): Result[TypeBinder] =
    val bound: Result[Type] = param.bound match
      case None => Right(Definitions.anyType.withPosFrom(param))
      case Some(tpe) => 
        checkType(tpe).flatMap: tpe1 =>
          val cs = tpe1.captureSet
          if TypeComparer.checkSubcapture(cs, CaptureSet.empty) then
            Right(tpe1)
          else
            Left(List(TypeError.GeneralError(s"Type parameter ${param.name} has a non-pure bound type: ${tpe1.show}, consider boxing it").withPos(param.pos)))
    bound.map: tpe =>
      val binder: TypeBinder = TypeBinder(param.name, tpe)
      binder.maybeWithPosFrom(param)

  def checkCaptureParam(param: Syntax.CaptureParam)(using ctx: Context): Result[CaptureBinder] =
    val bound: Result[CaptureSet] = param.bound match
      case None => Right(CaptureSet.universal.withPosFrom(param))
      case Some(captureSet) => checkCaptureSet(captureSet)
    bound.map: captureSet =>
      val binder: CaptureBinder = CaptureBinder(param.name, captureSet)
      binder.withPosFrom(param)

  def checkCaptureOrTypeParam(binder: (Syntax.CaptureParam | Syntax.TypeParam))(using ctx: Context): Result[CaptureBinder | TypeBinder] =
    binder match
      case binder: Syntax.CaptureParam => checkCaptureParam(binder)
      case binder: Syntax.TypeParam => checkTypeParam(binder)

  def checkCaptureRef(ref: Syntax.CaptureRef)(using ctx: Context): Result[QualifiedRef] =
    hopefully:
      val coreRef = checkCaptureRefCore(ref).!!
      QualifiedRef(ref.mode, coreRef).maybeWithPosFrom(ref)

  def checkCaptureRefCore(ref: Syntax.CaptureRef)(using ctx: Context): Result[CaptureRef] =
    if ref.name == "cap" then
      Right(CaptureRef.CAP().maybeWithPosFrom(ref))
    else lookupAll(ref.name) match
      case Some(sym: DefSymbol) => Right(CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))).maybeWithPosFrom(ref))
      case Some((binder: (Binder.CaptureBinder | Binder.TermBinder), idx)) => Right(CaptureRef.Ref(Type.Var(Term.BinderRef(idx))).maybeWithPosFrom(ref))
      case Some((binder: Binder.TypeBinder, idx)) => Left(List(TypeError.UnboundVariable(ref.name, s"I found a type name, but was looking for either a term or capture name").withPos(ref.pos)))
      case _ => Left(List(TypeError.UnboundVariable(ref.name).withPos(ref.pos)))

  def checkCaptureSet(captureSet: Syntax.CaptureSet)(using ctx: Context): Result[CaptureSet] =
    val checkElems: List[Result[QualifiedRef]] = captureSet.elems.map(checkCaptureRef)
    @annotation.tailrec
    def go(elems: List[Result[QualifiedRef]], acc: List[QualifiedRef]): Result[CaptureSet] = elems match
      case Nil => Right(CaptureSet(acc.reverse).maybeWithPosFrom(captureSet))
      case Left(error) :: _ => Left(error)
      case Right(elem) :: rest => go(rest, elem :: acc)
    go(checkElems, Nil)

  def lookupStructSymbol(name: String)(using ctx: Context): Option[StructSymbol] =
    lookupSymbol(name) match
      case Some(sym: StructSymbol) => Some(sym)
      case _ => None

  def findBaseType(name: String): Option[Type] = name match
    case "Unit" => Some(Definitions.unitType)
    case "Int" => Some(Definitions.intType)
    case "Any" => Some(Definitions.anyType)
    case "Nothing" => Some(Definitions.nothingType)
    case "i32" => Some(Definitions.i32Type)
    case "i64" => Some(Definitions.i64Type)
    case "bool" => Some(Definitions.boolType)
    case "array" => Some(Definitions.arrayConstructorType)
    case "char" => Some(Definitions.charType)
    case "Region" => Some(Definitions.regionType)
    case "RegionRef" => Some(Definitions.regionRefConstructorType)
    // case "String" => Some(Definitions.strType)
    // case "Break" => Some(Definitions.breakConstructorType)
    // Float types are not supported yet, so commenting them out for now
    // case "f32" => Some(Definitions.f32Type)
    // case "f64" => Some(Definitions.f64Type)
    case _ => None

  def computePeak(set: CaptureSet)(using Context): CaptureSet =
    def goRef(ref: QualifiedRef): Set[QualifiedRef] = 
      ref.core match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) =>
          getBinder(idx) match
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
        case CaptureRef.CapInst(_, _, _) => Set(ref)
    def goRefs(refs: List[QualifiedRef]): Set[QualifiedRef] =
      refs.flatMap(goRef).toSet
    val elems = goRefs(set.elems)
    CaptureSet(elems.toList)

  def checkSeparation(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSeparation ${cs1.show} ${cs2.show}"):
    boundary:
      val pk1 = computePeak(cs1).elems
      val pk2 = computePeak(cs2).elems
      for i <- 0 until pk1.length do
        for j <- 0 until pk2.length do
          if !checkSeparation(pk1(i), pk2(j)) then
            break(false)
      true

  def checkSeparation(ref1: QualifiedRef, ref2: QualifiedRef)(using Context): Boolean =
    def derivesFrom(ref1: CaptureRef, ref2: CaptureRef): Boolean =
      (ref1, ref2) match
        case (CaptureRef.CapInst(id1, _, from1), CaptureRef.CapInst(id2, _, from2)) => from1 == Some(id2)
        case _ => false
    val result = (ref1.isReadOnly && ref2.isReadOnly) || (ref1.core != ref2.core && !derivesFrom(ref1.core, ref2.core) && !derivesFrom(ref2.core, ref1.core))
    result

  def checkType(tpe: Syntax.Type)(using Context): Result[Type] = tpe match
    case Syntax.Type.Ident(name) => 
      def tryBaseType: Result[Type] = findBaseType(name) match
        case Some(baseType) => Right(baseType.maybeWithPosFrom(tpe))
        case None => Left(List(TypeError.UnboundVariable(name).withPos(tpe.pos)))
      def tryBinder: Result[Type] = lookupBinder(name) match
        case Some((binder: Binder.TypeBinder, idx)) => Right(Type.BinderRef(idx).withKind(TypeKind.Star).maybeWithPosFrom(tpe))
        case Some((binder: Binder, idx)) => 
          Left(List(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a type").maybeWithPosFrom(tpe)))
        case None => Left(List(TypeError.UnboundVariable(name).maybeWithPosFrom(tpe)))
      def trySymbol: Result[Type] = lookupSymbol(name) match
        case Some(sym: StructSymbol) => 
          val argVariances = sym.info.variances
          val kind =
            if argVariances.isEmpty then TypeKind.Star
            else TypeKind.Arrow(argVariances, TypeKind.Star)
          Right(Type.SymbolRef(sym).withKind(kind).maybeWithPosFrom(tpe))
        case Some(sym: TypeDefSymbol) =>
          val kind =
            if sym.info.typeParams.isEmpty then TypeKind.Star
            else TypeKind.Arrow(sym.info.variances, TypeKind.Star)
          Right(Type.SymbolRef(sym).withKind(kind).maybeWithPosFrom(tpe))
        case Some(sym: EnumSymbol) =>
          val argVariances = sym.info.variances
          val kind = if argVariances.isEmpty then TypeKind.Star else TypeKind.Arrow(argVariances, TypeKind.Star)
          Right(Type.SymbolRef(sym).withKind(kind).maybeWithPosFrom(tpe))
        case _ => Left(List(TypeError.UnboundVariable(name).maybeWithPosFrom(tpe)))
      tryBaseType || tryBinder || trySymbol
    case Syntax.Type.Arrow(params, result) =>
      def go(ps: List[Syntax.TermParam], acc: List[TermBinder])(using Context): Result[List[TermBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkTermParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkType(result)(using ctx.extend(params)).map: result1 =>
          Type.TermArrow(params, result1).maybeWithPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.TypeArrow(params, result) => 
      def go(ps: List[Syntax.TypeParam | Syntax.CaptureParam], acc: List[TypeBinder | CaptureBinder])(using Context): Result[List[TypeBinder | CaptureBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkCaptureOrTypeParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkType(result)(using ctx.extend(params)).map: result1 =>
          Type.TypeArrow(params, result1).maybeWithPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.Capturing(inner, isRO, captureSet) =>
      hopefully:
        val inner1 = checkType(inner).!!
        val captureSet1 = checkCaptureSet(captureSet).!!
        Type.Capturing(inner1, isRO, captureSet1).maybeWithPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.AppliedType(tycon, args) =>
      hopefully:
        val tycon1 = checkType(tycon).!!
        tycon1.kind match
          case TypeKind.Arrow(argVariances, resKind) =>
            if argVariances.length != args.length then
              sorry(TypeError.GeneralError(s"Number of type arguments mismatch: expected ${argVariances.length}, but got ${args.length}").withPos(tpe.pos))
            val targs1: List[Type | CaptureSet] = args.map: arg =>
              arg match
                case arg: Syntax.Type => checkType(arg).!!.boxIfImpure
                case arg: Syntax.CaptureSet => checkCaptureSet(arg).!!
            Type.AppliedType(tycon1, targs1).maybeWithPosFrom(tpe).withKind(resKind)
          case _ => sorry(TypeError.GeneralError("This is not a type constructor").withPos(tycon.pos))
    case Syntax.Type.Boxed(core) =>
      hopefully:
        val core1 = checkType(core).!!
        if core1.isPure then core1
        else Type.Boxed(core1).withPosFrom(tpe)
    case Syntax.Type.Splice(tp) => Right(tp.withPosFrom(tpe))

  def checkTermParamList(params: List[Syntax.TermParam], pts: Option[List[Type]] = None, srcPos: SourcePos)(using Context): Result[List[TermBinder]] =
    hopefully:
      var result: ArrayBuffer[TermBinder] = ArrayBuffer.empty
      var nowCtx: Context = ctx
      val expectedTypes = pts match
        case Some(pts) => pts
        case None => params.map(_ => Type.NoType())
      if expectedTypes.length != params.length then
        sorry(TypeError.TypeMismatch(s"a lambda of ${expectedTypes.length} parameters", s"a lambda of ${params.length} parameters").withPos(srcPos))
      for (p, tpe) <- params zip expectedTypes do
        val bd = checkTermParam(p, tpe)(using nowCtx).!!
        result += bd
        nowCtx = nowCtx.extend(bd)
      result.toList

  def checkTypeParamList(params: List[Syntax.TypeParam | Syntax.CaptureParam])(using Context): Result[List[TypeBinder | CaptureBinder]] =
    def go(ps: List[Syntax.TypeParam | Syntax.CaptureParam], acc: List[TypeBinder | CaptureBinder])(using Context): Result[List[TypeBinder | CaptureBinder]] = ps match
      case Nil => Right(acc.reverse)
      case p :: ps =>
        checkCaptureOrTypeParam(p).flatMap: binder =>
          go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
    go(params, Nil)

  private def dropLocalParams(crefs: List[QualifiedRef], numParams: Int, paramCapInsts: List[CaptureRef.CapInst]): (Boolean, List[QualifiedRef]) = 
    var existsLocalParams = false
    val newCrefs = crefs.flatMap: ref =>
      ref.core match
        case CaptureRef.Ref(singleton) =>
          val (root, recover) = getRoot(singleton)
          root match
            case Term.BinderRef(idx) =>
              if idx >= numParams then
                Some(QualifiedRef(ref.mode, CaptureRef.Ref(recover(Term.BinderRef(idx - numParams))).maybeWithPosFrom(ref)))
              else 
                existsLocalParams = true
                None
            case _ => Some(ref)
        case capInst: CaptureRef.CapInst if paramCapInsts.contains(capInst) => None  // Drop cap instances that comes from parameters
        case _ => Some(ref)
    (existsLocalParams, newCrefs)

  def checkTypeArgs(targs: List[Syntax.Type | Syntax.CaptureSet], formals: List[TypeBinder | CaptureBinder], srcPos: SourcePos)(using Context): Result[List[Type | CaptureSet]] =
    val formalTypes: List[Type | CaptureSet] = formals.map:
      case TypeBinder(name, bound) => bound
      case CaptureBinder(name, bound) => bound
    hopefully:
      def go(ts: List[Syntax.Type | Syntax.CaptureSet], fs: List[Type | CaptureSet], checkedAcc: List[Type | CaptureSet])(using Context): List[Type | CaptureSet] = (ts, fs) match
        case (Nil, Nil) => checkedAcc.reverse
        case (t :: ts, f :: fs) =>
          val typeArg: (Type | CaptureSet) = 
            (t, f) match
              case (t: Syntax.Type, f: Type) =>
                val formal1 = substituteType(f, checkedAcc.reverse, isParamType = true)
                var tpe = checkType(t).!!
                if !tpe.isPure then
                  tpe = Type.Boxed(tpe)
                if !TypeComparer.checkSubtype(tpe, formal1) then
                  sorry(TypeError.GeneralError(s"Type argument ${tpe.show} does not conform to the bound ${f.show}").withPosFrom(t))
                tpe
              case (t: Syntax.CaptureSet, f: CaptureSet) => 
                val f1 = substituteTypeInCaptureSet(f, checkedAcc.reverse, isParamType = true)
                val cs1 = checkCaptureSet(t).!!
                if f.elems.exists(_.isUniversal) || TypeComparer.checkSubcapture(cs1, f1) then
                  cs1
                else sorry(TypeError.GeneralError(s"Capture set argument ${cs1.show} does not conform to the bound ${f.show}").withPosFrom(t))
              case (t, f) => sorry(TypeError.GeneralError("Argument kind mismatch").withPosFrom(t))
          go(ts, fs, typeArg :: checkedAcc)
        case _ => 
          sorry(TypeError.GeneralError(s"Type argument number mismatch: expected ${formals.length}, but got ${targs.length}").withPos(srcPos))
      go(targs, formalTypes, Nil)

  def instantiateCaps(tpe: Type, isFresh: Boolean, fromInst: Option[CaptureRef.CapInst] = None)(using Context): (Type, List[CaptureRef.CapInst]) =
    var createdCaps: List[CaptureRef.CapInst] = Nil
    val curLevel = ctx.freshLevel
    val capKind = if isFresh then CapKind.Fresh(curLevel) else CapKind.Sep(curLevel)
    val maybeFromInst = fromInst.map(_.capId)
    val tm1 = CapInstantiation(() => CaptureRef.makeCapInst(capKind, fromInst = maybeFromInst))
    val tpe1 = tm1.apply(tpe)
    createdCaps = tm1.localCaps
    val rootCapInst = tpe1.captureSet match
      case CaptureSet.Const((QualifiedRef(_, c: CaptureRef.CapInst)) :: Nil) if tm1.localCaps.contains(c) => Some(c)
      case _ => None
    val fieldNames = allFieldNames(tpe1)
    val fieldInfos: List[FieldInfo] = 
      rootCapInst match
        case None => Nil
        case Some(rootCap) =>
          fieldNames.flatMap: fieldName =>
            val fieldInfo = getFieldInfo(tpe1, fieldName).get
            val fieldCapKind = capKind  // fields inherit the fresh-ness status from its root cap
            val tm2 = CapInstantiation(() => CaptureRef.makeCapInst(fieldCapKind, fromInst = Some(rootCap.capId)))
            val fieldType = tm2.apply(fieldInfo.tpe)
            createdCaps = createdCaps ++ tm2.localCaps
            if tm2.localCaps.isEmpty then
              None
            else
              val info = FieldInfo(fieldName, fieldType, fieldInfo.mutable)
              Some(info)
    if fieldInfos.isEmpty then (tpe1, createdCaps)
    else (tpe1.refined(fieldInfos), createdCaps)

  def instantiateBinderCaps(binder: TermBinder)(using Context): (TermBinder, List[CaptureRef.CapInst]) =
    val (tpe1, createdCaps) = instantiateCaps(binder.tpe, isFresh = binder.isConsume)
    val binder1 = TermBinder(binder.name, tpe1, binder.isConsume).maybeWithPosFrom(binder).asInstanceOf[TermBinder]
    (binder1, createdCaps)

  def checkStructInit(
    classSym: StructSymbol, 
    targs: Either[List[Syntax.Type | Syntax.CaptureSet], List[Type | CaptureSet]], 
    args: List[Syntax.Term], 
    expected: Type,
    srcPos: SourcePos,
    onRegion: Boolean = false)(using Context): Result[Term] =
    val tformals = classSym.info.targs
    val fields = classSym.info.fields
    hopefully:
      if args.length != fields.length then
        sorry(TypeError.GeneralError(s"Constructor argument number mismatch, expected ${fields.length}, but got ${args.length}").withPos(srcPos))
      val typeArgs = targs match
        case Left(targs) => 
          if targs.length != tformals.length then
            sorry(TypeError.GeneralError(s"Constructor type argument number mismatch, expected ${tformals.length}, but got ${targs.length}").withPos(srcPos))
          checkTypeArgs(targs, tformals, srcPos).!!
        case Right(targs) => targs
      val fields1: List[(String, Type, Boolean)] = fields.map: field =>
        val tpe = field.tpe
        val fieldType = substituteType(tpe, typeArgs, isParamType = true)
        (field.name, fieldType, field.mutable)
      val argFormals = fields1.map(_._2)
      val syntheticFunctionType = Type.TermArrow(argFormals.map(tpe => TermBinder("_", tpe, isConsume = false)), Definitions.anyType)
      val (termArgs, _, consumedSet) = checkFunctionApply(syntheticFunctionType, args, srcPos, isDependent = false).!!
      var classType = 
        if typeArgs.isEmpty then
          Type.SymbolRef(classSym)
        else
          Type.AppliedType(Definitions.structConstructorType(classSym), typeArgs)
      val termCaptureElems = termArgs.flatMap: arg =>
        arg.maybeAsSingletonType match
          case Some(singleton) => QualifiedRef(AccessMode.Normal(), singleton.asCaptureRef) :: Nil
          case None => arg.tpe.captureSet.elems
      val refinements: List[FieldInfo] = (fields1 `zip` termArgs).flatMap: 
        case ((name, fieldType, mutable), arg) =>
          if fieldType.isPure then None
          else Some(FieldInfo(name, arg.tpe, mutable))
      val captureSet = CaptureSet.universal ++ CaptureSet(termCaptureElems)
      if onRegion then
        classType = Definitions.regionRefType(classType)
      val outType = Type.Capturing(classType, isReadOnly = false, captureSet)
      val refinedOutType = outType.refined(refinements)
      Term.StructInit(classSym, typeArgs, termArgs).withPos(srcPos).withTpe(refinedOutType).withCVFrom(termArgs*)

  def dropLocalFreshCaps(crefs: List[QualifiedRef], srcPos: SourcePos)(using Context): Result[List[QualifiedRef]] =
    hopefully:
      val freshLevel = ctx.freshLevel
      crefs.filter: cref =>
        cref.core match
          case CaptureRef.CapInst(capId, CapKind.Fresh(level), fromInst) =>
            level < freshLevel
          case CaptureRef.CapInst(capId, CapKind.Sep(level), fromInst) =>
            if level >= freshLevel then
              sorry(TypeError.GeneralError(s"A local `cap` instance is leaked into the body of a lambda").withPos(srcPos))
            true
          case _ => true

  def inNewFreshLevel[T](body: Context ?=> T)(using Context): T =
    val nowCtx = ctx.newFreshLevel
    body(using nowCtx)
      
  def checkTermAbstraction(params: List[TermBinder], checkBody: Context ?=> Result[Term], srcPos: SourcePos)(using Context): Result[Term] = 
    inNewFreshLevel:
      hopefully:
        val (params1, createdCaps) = params.map(instantiateBinderCaps).unzip
        val ctx1 = ctx.extend(params1)
        val body1 = checkBody(using ctx1).!!
        val bodyCV = body1.cv
        if bodyCV.elems.contains(CaptureRef.CAP()) then
          sorry(TypeError.GeneralError("A `cap` that is not nameable is captured by the body of this lambda; try naming `cap`s explicitly with capture parameters").withPos(srcPos))
        val (_, outCV) = dropLocalParams(bodyCV.elems.toList, params.length, createdCaps.flatten)
        val outCV1 = dropLocalFreshCaps(outCV, srcPos).!!
        val outTerm = Term.TermLambda(params, body1, skolemizedBinders = params1).withPos(srcPos)
        val outType = Type.Capturing(Type.TermArrow(params, body1.tpe), isReadOnly = false, CaptureSet(outCV1.distinct))
        outTerm.withTpe(outType).withCV(CaptureSet.empty)

  def checkTypeAbstraction(params: List[TypeBinder | CaptureBinder], checkBody: Context ?=> Result[Term], srcPos: SourcePos)(using Context): Result[Term] =
    inNewFreshLevel:
      hopefully:
        val ctx1 = ctx.extend(params)
        val body1 = checkBody(using ctx1).!!
        val bodyCV = body1.cv
        if bodyCV.elems.contains(CaptureRef.CAP()) then
          sorry(TypeError.GeneralError("A `cap` that is not nameable is captured by the body of this lambda; try naming `cap`s explicitly with capture parameters").withPos(srcPos))
        val (existsLocalParams, outCV) = dropLocalParams(bodyCV.elems.toList, params.length, Nil)
        if existsLocalParams then
          sorry(TypeError.GeneralError("local capture parameters captured by the body of a the lambda"))
        val outCV1 = dropLocalFreshCaps(outCV, srcPos).!!
        val outTerm = Term.TypeLambda(params, body1).withPos(srcPos)
        val outType = Type.Capturing(Type.TypeArrow(params, body1.tpe), isReadOnly = false, CaptureSet(outCV1))
        outTerm.withTpe(outType).withCV(CaptureSet.empty)

  def adaptLazyType(t: Term)(using Context): Term =
    t.tpe.stripCaptures match
      case LazyType(resultType) =>
        val t1 = Term.TypeApply(t, Nil).withPosFrom(t)
        val outTerm = t1.withTpe(resultType).withCV(t.cv)
        instantiateFresh(outTerm)
      case _ => t

  def maybeAdaptUnit(t: Term, expected: Type)(using Context): Term =
    if expected.strip == Definitions.unitType && !TypeComparer.checkSubtype(t.tpe, expected) then
      // Adapts `t` to `val _ = t in ()`
      val outTerm = Term.Bind(TermBinder("_", t.tpe, isConsume = false), recursive = false, t, makeUnitLit(t.pos))
      outTerm.withTpe(expected).withCV(t.cv).withPosFrom(t)
    else
      // Do nothing
      t

  def maybeAdaptBoxed(t: Term, expected: Type)(using Context): Result[Term] =
    hopefully:
      if expected.exists && !TypeComparer.checkSubtype(t.tpe, expected) then
        if t.tpe.isBoxedType && !expected.isBoxedType then
          checkUnbox(t, t.pos).!!
        else if !t.tpe.isBoxedType && expected.isBoxedType then
          checkBox(t, t.pos).!!
        else t
      else t

  def checkIdent(t: Syntax.Term.Ident, pt: Type = Type.NoType())(using Context): Result[Term] =
    def tryLookup: Result[Term] =
      hopefully:
        val (ref, tpe) = lookupAll(t.name) match
          case Some(sym: DefSymbol) => (Term.SymbolRef(sym): VarRef, sym.tpe)
          case Some((binder: Binder.TermBinder, idx)) => (Term.BinderRef(idx): VarRef, binder.tpe)
          case Some((binder: Binder, idx)) => sorry(TypeError.UnboundVariable(t.name, s"I found a ${binder.kindStr} name, but was looking for a term").withPos(t.pos))
          case _ => sorry(TypeError.UnboundVariable(t.name).withPos(t.pos))
        val cv = if !tpe.isPure then ref.asSingletonType.singletonCaptureSet.withPos(t.pos) else CaptureSet.empty
        val tpe1 = 
          if tpe.isPure then 
            tpe 
          else 
            val core = tpe.stripCaptures
            tpe.derivedCapturing(core, tpe.isReadOnly, ref.asSingletonType.singletonCaptureSet)
        ref.withPosFrom(t).withTpe(tpe1).withCV(cv)
    def tryEtaExpandPrimitive: Result[Term] =
      hopefully:
        if PrimitiveOp.fromName(t.name).isDefined then
          pt match
            case TermFunctionType(params, resultType) =>
              val term1 = Synthetics.etaExpand(t, params)
              checkTerm(term1, pt).!!
            case _ => sorry(TypeError.UnboundVariable(t.name, "this is the name of a primitive operator").withPos(t.pos))
        else sorry(TypeError.UnboundVariable(t.name).withPos(t.pos))
    def tryBuiltins: Result[Term] =
      hopefully:
        if t.name == "???" then
          val outTerm = Term.PrimOp(PrimitiveOp.Sorry, Nil, Nil).withPosFrom(t).withCV(CaptureSet.empty).withTpe(Definitions.nothingType)
          outTerm
        else sorry(TypeError.UnboundVariable(t.name).withPos(t.pos))
    tryBuiltins || tryEtaExpandPrimitive || tryLookup

  def destructMatchableType(scrutineeType: Type, srcPos: SourcePos)(using Context): Result[(EnumSymbol | StructSymbol, List[Type | CaptureSet])] =
    hopefully:
      def go(tpe: Type): (EnumSymbol | StructSymbol, List[Type | CaptureSet]) =
        tpe match
          case Type.RefinedType(base, _) => go(base)
          case Type.SymbolRef(sym: EnumSymbol) => (sym, Nil)
          case Type.SymbolRef(sym: StructSymbol) => (sym, Nil)
          case Type.AppliedType(base, targs) =>
            val (sym, targs1) = go(base)
            (sym, targs1 ++ targs)
          case _ => sorry(TypeError.GeneralError(s"cannot match on a non-`enum` and non-`struct` type ${scrutineeType.show}").withPos(srcPos))
      go(scrutineeType.simplify)

  def checkPattern(pat: Syntax.Pattern, scrutineeType: Type)(using Context): Result[Pattern] =
    hopefully:
      pat match
        case Syntax.Pattern.Wildcard() => 
          Pattern.Wildcard().withPosFrom(pat).withTpe(scrutineeType)
        case Syntax.Pattern.Bind(name, pat) => 
          val pat1 = checkPattern(pat, scrutineeType).!!
          val binder1 = TermBinder(name, scrutineeType, isConsume = false).withPosFrom(pat)
          Pattern.Bind(binder1.asInstanceOf[TermBinder], pat1).withPosFrom(pat).withTpe(scrutineeType)
        case Syntax.Pattern.EnumVariant(constructor, fields) => 
          val (enumSym, typeArgs) = destructMatchableType(scrutineeType, pat.pos).!!
          val variantSymbol = lookupStructSymbol(constructor) match
            case None => sorry(TypeError.UnboundVariable(constructor).withPos(pat.pos))
            case Some(sym: StructSymbol) => sym
          enumSym match
            case enumSym: EnumSymbol =>
              if !enumSym.info.variants.exists(_ eq variantSymbol) then
                sorry(TypeError.GeneralError(s"not a valid variant name: $constructor").withPos(pat.pos))
            case classSym: StructSymbol =>
              if !(classSym eq variantSymbol) then
                sorry:
                  val err = TypeError.GeneralError:
                    s"match case is disjoint: scrutinee is of type ${scrutineeType.show} but the pattern expects a struct of ${variantSymbol.name}"
                  err.withPos(pat.pos)
          val isNarrowing = !(enumSym eq variantSymbol)
          val numFields = variantSymbol.info.fields.length
          if fields.length != numFields then
            sorry(TypeError.GeneralError(s"expected ${numFields} fields, but got ${fields.length}").withPos(pat.pos))
          val fieldTypes = 
            if isNarrowing then
              variantSymbol.info.fields.map: fieldInfo =>
                val fieldType = substituteType(fieldInfo.tpe, typeArgs, isParamType = false)
                fieldType
            else
              variantSymbol.info.fields.map: fieldInfo =>
                getFieldInfo(scrutineeType, fieldInfo.name) match
                  case Some(fieldInfo) => fieldInfo.tpe
                  case None => assert(false)
          val fieldPatterns = (fields `zip` fieldTypes).map: (fieldPat, fieldType) =>
            checkPattern(fieldPat, fieldType).!!
          val enumSym1 = enumSym match
            case enumSym: EnumSymbol => Some(enumSym)
            case _ => None
          Pattern.EnumVariant(variantSymbol, typeArgs, enumSym1, fieldPatterns).withPosFrom(pat).withTpe(scrutineeType)

  def bindersInPattern(pat: Pattern): List[TermBinder] =
    pat match
      case Pattern.Wildcard() => Nil
      case Pattern.Bind(binder, pat) => binder :: bindersInPattern(pat)
      case Pattern.EnumVariant(_, _, _, fields) => fields.flatMap(bindersInPattern)

  /** Given `tp1` and `tp2` who are not subtypes of each other, compute their least upper bound. */
  def computeLub(tp1: Type, tp2: Type)(using Context): Option[Type] = //trace(s"computeLub(${tp1.show}, ${tp2.show})"):
    def trySame: Option[Type] = if tp1 == tp2 then Some(tp1) else None
    def tryStripCaptures: Option[Type] =
      val tp11 = tp1.stripCaptures
      val tp21 = tp2.stripCaptures
      if !((tp11 eq tp1) && (tp21 eq tp21)) then
        computeLub(tp11, tp21).map: core => 
          tp1.derivedCapturing(core, tp1.isReadOnly || tp2.isReadOnly, tp1.captureSet ++ tp2.captureSet)
      else None
    def tryStripRefinements: Option[Type] =
      val tp11 = tp1.stripRefinements
      val tp21 = tp2.stripRefinements
      if !((tp11 eq tp1) && (tp21 eq tp2)) then
        computeLub(tp11, tp21)
      else None
    def tryEnum: Option[Type] =
      (tp1, tp2) match
        case (AppliedEnumType(enumSym1, targs1), AppliedStructType(structSym2, targs2)) if structSym2.info.enumSymbol == Some(enumSym1) && targs1 == targs2 =>
          Some(tp1)
        case (AppliedStructType(structSym1, targs1), AppliedEnumType(enumSym2, targs2)) if structSym1.info.enumSymbol == Some(enumSym2) && targs1 == targs2 =>
          Some(tp2)
        case (AppliedStructType(structSym1, targs1), AppliedStructType(structSym2, targs2)) if targs1 == targs2 =>
          if structSym1.info.enumSymbol.isDefined && structSym1.info.enumSymbol == structSym2.info.enumSymbol then
            Some(AppliedEnumType(structSym1.info.enumSymbol.get, targs1))
          else None
        case _ => None
    trySame `orElse` tryStripCaptures `orElse` tryStripRefinements `orElse` tryEnum

  def findCommonType(tp1: Type, tp2: Type)(using Context): Option[Type] =
    if TypeComparer.checkSubtype(tp1, tp2) then
      Some(tp2)
    else if TypeComparer.checkSubtype(tp2, tp1) then
      Some(tp1)
    else
      computeLub(tp1, tp2)

  def findCommonTypeAmong(tpes: List[Type])(using Context): Option[Type] = boundary:
    var result: Type = Definitions.nothingType
    for tp <- tpes do
      findCommonType(result, tp) match
        case Some(tp1) => result = tp1
        case None => break(None)
    Some(result)

  def checkMatchCase(pat: Syntax.MatchCase, scrutineeType: Type, expected: Type)(using Context): Result[MatchCase] =
    hopefully:
      val pat1 = checkPattern(pat.pat, scrutineeType).!!
      val binders = bindersInPattern(pat1)
      val binders1 = binders.zipWithIndex.map: (oldBinder, idx) =>
        val oldTpe = oldBinder.tpe
        val newTpe = oldTpe.shift(idx)
        oldBinder.copy(tpe = newTpe).withPosFrom(oldBinder)
      val ctx1 = ctx.extend(binders1)
      val body1 = checkTerm(pat.body)(using ctx1).!!
      // Avoid locally-bound binders
      var outCV = body1.cv
      var outType = body1.tpe
      for binder <- binders.reverse do
        val tm = AvoidLocalBinder(binder.tpe)
        outCV = tm.mapCaptureSet(outCV)
        outType = tm.apply(outType)
      MatchCase(pat1, body1).withPosFrom(pat).withTpe(outType).withCV(outCV)

  def checkMatch(scrutinee: Syntax.Term, cases: List[Syntax.MatchCase], expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val scrutinee1 = checkTerm(scrutinee).!!
      destructMatchableType(scrutinee1.tpe, scrutinee.pos).!!   // sanity check
      val cases1 = cases.map: cas =>
        checkMatchCase(cas, scrutinee1.tpe, expected).!!
      val outType = 
        if expected.exists then 
          expected 
        else
          findCommonTypeAmong(cases1.map(_.tpe)) match
            case Some(tp) => tp
            case None => 
              sorry(TypeError.GeneralError("Cannot find a common upper type for all match cases").withPos(srcPos))
      val outTerm = Term.Match(scrutinee1, cases1).withPos(srcPos).withTpe(outType).withCVFrom(scrutinee1 :: cases1*)
      outTerm

  def checkTerm(t: Syntax.Term, expected: Type = Type.NoType())(using Context): Result[Term] = 
    def trySpecialForms: Result[Option[Term]] = hopefully:
      t match
        case Syntax.Term.Apply(sel @ Syntax.Term.Select(base, field), args) =>
          val base1 = checkTerm(base).!!
          if base1.tpe.isRegionType then
            val peaks = computePeak(base1.tpe.captureSet)
            val regionPeak: CaptureRef.CapInst = peaks.elems match
              case (QualifiedRef(_, inst: CaptureRef.CapInst)) :: Nil => inst
              case _ => sorry(TypeError.GeneralError(s"Invalid region with type ${base1.tpe.show}"))
            val classSym = lookupStructSymbol(field) match
              case None => sorry(TypeError.GeneralError(s"Regions only allocate `struct`s, but $field is not a struct").withPos(sel.pos))
              case Some(sym) => sym
            val initTerm = checkStructInit(classSym, Left(Nil), args, expected, t.pos, onRegion = true).!!
            val outTerm = Term.PrimOp(PrimitiveOp.RegionAlloc, Nil, List(base1, initTerm)).like(initTerm).withMoreCV(base1.cv)
            instantiateFresh(outTerm, fromInst = Some(regionPeak))
            Some(outTerm)
          else None
        case _ => None
    def tryDefault: Result[Term] = t match
      case t: Syntax.Term.Ident =>
        hopefully:
          val outTerm = checkIdent(t, expected).!!
          val cv = outTerm.cv
          ctx.consumedPeaks.elems.foreach: cref =>
            if !checkSeparation(cv, CaptureSet(cref :: Nil)) then
              val consumedPos = if cref.hasPos then Some(cref.pos) else None
              sorry(TypeError.ConsumedError(cv.show, consumedPos).withPos(t.pos))
          if expected.isReadOnly then
            outTerm.setTpe(outTerm.tpe.asReadOnly)
            outTerm.setCV(outTerm.cv.qualify(AccessMode.ReadOnly()))
          outTerm
      case Syntax.Term.StrLit(value) => 
        val charArrayType = Definitions.arrayType(Definitions.charType)
        Right(Term.StrLit(value).withPosFrom(t).withTpe(charArrayType).withCV(CaptureSet.empty))
      case Syntax.Term.CharLit(value) =>
        Right(Term.CharLit(value).withPosFrom(t).withTpe(Definitions.charType).withCV(CaptureSet.empty))
      case Syntax.Term.IntLit(value) => 
        val tpe = if expected.exists && expected.isIntegralType then expected else Definitions.i32Type
        Right(Term.IntLit(value).withPosFrom(t).withTpe(tpe).withCV(CaptureSet.empty))
      case Syntax.Term.BoolLit(value) =>
        val tpe = Definitions.boolType
        Right(Term.BoolLit(value).withPosFrom(t).withTpe(tpe).withCV(CaptureSet.empty))
      case Syntax.Term.UnitLit() => 
        Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType).withCV(CaptureSet.empty))
      case t @ Syntax.Term.Select(base, field) =>
        // checkStablePath(t) || 
        checkSelect(base, field, t.pos)
      case t @ Syntax.Term.TypeApply(inner @ Syntax.Term.Select(base, field), targs) =>
        checkSelect(base, field, t.pos, targs)
      case Syntax.Term.Assign(lhs, rhs) =>
        checkAssign(lhs, rhs, t.pos)
      case Syntax.Term.Infix(op, lhs, rhs) =>
        checkInfix(op, lhs, rhs, expected, t.pos)
      case Syntax.Term.Prefix(op, term) =>
        checkPrefix(op, term, expected, t.pos)
      case Syntax.Term.If(cond, thenBranch, maybeElseBranch) =>
        checkIf(cond, thenBranch, maybeElseBranch, expected, t.pos)
      case Syntax.Term.Match(scrutinee, cases) =>
        checkMatch(scrutinee, cases, expected, t.pos)
      case Syntax.Term.Lambda(params, body) => 
        hopefully:
          val pts = expected match
            case TermFunctionType(params, _) => Some(params.map(_.tpe))
            case _ => None
          val ps1 = checkTermParamList(params, pts, t.pos).!!
          def checkBody(using Context): Result[Term] =
            checkTerm(body)
          checkTermAbstraction(ps1, checkBody, t.pos).!!
      case Syntax.Term.TypeLambda(params, body) =>
        hopefully:
          val params1 = checkTypeParamList(params).!!
          val maybeTypeBinder = params1.find:
            case binder: TypeBinder => true
            case _ => false
          maybeTypeBinder match
            case Some(binder) =>
              sorry(TypeError.GeneralError("Only capture parameters are allowed in first-class type lambdas").withPos(binder.pos))
            case None =>
          def checkBody(using Context): Result[Term] =
            checkTerm(body)
          checkTypeAbstraction(params1, checkBody, t.pos).!!
      case Syntax.Term.Block(stmts) => 
        def avoidSelfType(tpe: Type, d: Syntax.Definition): Result[Type] =
          val approxType = Type.Capturing(tpe.stripCaptures, isReadOnly = false, CaptureSet.universal)
          val tm = AvoidLocalBinder(approxType)
          val result = tm.apply(tpe)
          //println(s"avoidSelfType $tpe => $result")
          hopefully:
            if tm.ok then result else sorry(TypeError.GeneralError(s"Cannot avoid self type").withPos(d.pos))
        def go(stmts: List[Syntax.Definition | Syntax.Term])(using Context): Result[Term] = 
          def goDefinition(d: Syntax.Definition)(using Context): Result[(TermBinder, Term, Boolean)] =
            hopefully:
              val selfType = d match
                case _: Syntax.Definition.ValDef => None
                case d: Syntax.Definition.DefDef => Some(extractDefType(d, requireExplictType = false).!!)
                case _: Syntax.Definition.StructDef =>
                  sorry(TypeError.GeneralError("structs are not allowed in a block").withPos(d.pos))
                case _: Syntax.Definition.ExtensionDef =>
                  sorry(TypeError.GeneralError("extension definitions are not allowed in a block").withPos(d.pos))
                case _: Syntax.Definition.TypeDef =>
                  sorry(TypeError.GeneralError("type definitions are not allowed in a block").withPos(d.pos))
                case _: Syntax.Definition.EnumDef =>
                  sorry(TypeError.GeneralError("enum definitions are not allowed in a block").withPos(d.pos))
              val ctx1 = selfType match
                case None => ctx
                case Some(selfType) =>
                  val selfBinder = TermBinder(d.name, selfType, isConsume = false).withPos(d.pos)
                  ctx.extend(selfBinder)
              val (bd, expr) = checkDef(d.asInstanceOf[Syntax.ValueDef])(using ctx1).!!
              // Avoid self reference
              val bd1 = 
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(bd.tpe, d).!!
                  TermBinder(bd.name, tpe1, isConsume = false).withPos(bd.pos).asInstanceOf[TermBinder]
                else bd
              // Avoid self type in the body
              val expr1 =
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(expr.tpe, d).!!
                  expr.withTpe(tpe1)
                else expr
              (bd1, expr1, selfType.isDefined)
          stmts match
            case Nil => 
              Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType).withCV(CaptureSet.empty))
            case (t: Syntax.Term) :: Nil => 
              checkTerm(t)
            case (d: Syntax.Definition) :: Nil =>
              hopefully:
                val (bd1, expr1, isRecursive) = goDefinition(d).!!
                val retType = Definitions.unitType
                Term.Bind(
                  bd1, 
                  recursive = isRecursive, 
                  expr1, 
                  Term.UnitLit().withPosFrom(d).withTpe(retType).withCV(CaptureSet.empty)).withPosFrom(d).withTpe(retType).withCVFrom(expr1)
            case d :: ds =>
              val d1 = d match
                case d: Syntax.Definition => d
                case t: Syntax.Term => Syntax.Definition.ValDef(Fresh.freshName("_"), None, t).withPosFrom(t)
              hopefully:
                // typecheck the binder
                val (bd1, boundExpr1, isRecursive) = goDefinition(d1).!!

                // typecheck the body
                val peaksConsumed = consumedPeaks(boundExpr1.cv)
                val bodyExpr = go(ds)(using ctx.extend(bd1 :: Nil).moreConsumedPeaks(peaksConsumed.shift(1))).!!

                val resType = bodyExpr.tpe
                val approxElems = bd1.tpe.captureSet.elems.flatMap: cref =>
                  // if bd1.localCapInsts.contains(cref) then 
                  //   // drop local cap instances, since they mean "fresh" things
                  //   None
                  // else Some(cref)
                  Some(cref)
                val approxCs = CaptureSet(approxElems)
                val approxType = Type.Capturing(boundExpr1.tpe.stripCaptures, isReadOnly = false, approxCs)
                val tm = AvoidLocalBinder(approxType)
                val resType1 = tm.apply(resType)
                val bodyCV = bodyExpr.cv
                val outCV = tm.mapCaptureSet(bodyCV) ++ boundExpr1.cv
                if tm.ok then
                  Term.Bind(bd1, recursive = isRecursive, boundExpr1, bodyExpr).withPosFrom(d, bodyExpr).withTpe(resType1).withCV(outCV)
                else
                  sorry(TypeError.LeakingLocalBinder(resType.show(using ctx.extend(bd1 :: Nil))).withPos(d.pos))
        go(stmts)
      case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if PrimitiveOp.fromName(name).isDefined => 
        checkPrimOp(PrimitiveOp.fromName(name).get, args, expected, t.pos).map(instantiateFresh(_))
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if PrimitiveOp.fromName(name).isDefined =>
        val primOp = PrimitiveOp.fromName(name).get
        checkPolyPrimOp(primOp, targs, args, expected, t.pos).map(instantiateFresh(_))
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if lookupStructSymbol(name).isDefined =>
        val classSym = lookupStructSymbol(name).get
        checkStructInit(classSym, Left(targs), args, expected, t.pos).map(instantiateFresh(_))
      case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if lookupStructSymbol(name).isDefined =>
        withinNewInferenceScope:
          hopefully:
            val classSym = lookupStructSymbol(name).get
            val classTypeParams = classSym.info.targs
            val targs: Either[List[Syntax.Type | Syntax.CaptureSet], List[Type | CaptureSet]] =
              if classTypeParams.isEmpty then Left(Nil)
              else
                Right:
                  classTypeParams.map:
                    case binder: TypeBinder => 
                      val bound = if TypeComparer.checkSubtype(Definitions.anyType, binder.bound) then Type.NoType() else binder.bound
                      val tv = Inference.createTypeVar(binder.name, t.pos, bound)
                      tv
                    case binder: CaptureBinder => CaptureSet.empty
            val outTerm = checkStructInit(classSym, targs, args, expected, t.pos).!!
            Inference.solveTypeVars()
            Inference.state.localVars.foreach: tv =>
              val inst = tv.instance
              if !inst.isPure then
                sorry(TypeError.ImpureInferredType(inst.show).withPos(t.pos))
            val outTerm1 = instantiateFresh(outTerm)
            outTerm1
      case Syntax.Term.Apply(fun, args) => 
        hopefully:
          val fun1 = checkTerm(fun).!!
          fun1.tpe match
            case TypeFunctionType(binders, _) =>
              // Apply type function to type arguments
              withinNewInferenceScope:
                val targs: List[Type | CaptureSet] = binders.map:
                  case binder: TypeBinder =>
                    val bound = if TypeComparer.checkSubtype(Definitions.anyType, binder.bound) then Type.NoType() else binder.bound
                    val tv = Inference.createTypeVar(binder.name, t.pos, bound)
                    tv
                  case binder: CaptureBinder => CaptureSet.empty  // TODO: capture inference is not supported yet
                val fun2 = checkTypeApply(fun1, Right(targs), t.pos).!!
                val appliedTerm = checkApply(fun2, args, expected, t.pos).map(instantiateFresh(_)).!!
                Inference.solveTypeVars()
                for tv <- Inference.state.localVars do
                  if !tv.instance.isPure then
                    sorry(TypeError.ImpureInferredType(tv.instance.show).withPos(t.pos))
                appliedTerm
            case _ =>
              checkApply(fun1, args, expected, t.pos).map(instantiateFresh(_)).!!
      case Syntax.Term.TypeApply(term, targs) => 
        hopefully:
          val term1 = checkTerm(term).!!
          checkTypeApply(term1, Left(targs), t.pos).!!

    hopefully:
      val result = trySpecialForms.!!.getOrElse(tryDefault.!!)
      var outTerm = result
      outTerm = adaptLazyType(outTerm)
      outTerm = maybeAdaptUnit(outTerm, expected)
      outTerm = maybeAdaptBoxed(outTerm, expected).!!
      if !expected.exists || TypeComparer.checkSubtype(outTerm.tpe, expected) then
        outTerm
      else 
        sorry(TypeError.TypeMismatch(expected.show, outTerm.tpe.show).withPos(t.pos))

  def checkIf(
    cond: Syntax.Term, 
    thenBranch: Syntax.Term, 
    maybeElseBranch: Option[Syntax.Term], 
    expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val elseBranch = maybeElseBranch match
        case Some(elseBranch) => elseBranch
        case None =>
          Syntax.Term.UnitLit().withPos(srcPos)
      val expected1 = maybeElseBranch match
        case Some(_) => expected
        case None => Definitions.unitType
      val cond1 = checkTerm(cond, expected = Definitions.boolType).!!
      val thenBranch1 = checkTerm(thenBranch, expected = expected1).!!
      //val expected2 = if expected1.exists then expected1 else thenBranch1.tpe
      val elseBranch1 = checkTerm(elseBranch, expected = expected1).!!
      val finalTpe = findCommonType(thenBranch1.tpe, elseBranch1.tpe) match
        case Some(tpe) => tpe
        case None => sorry(TypeError.GeneralError("Cannot find a common type for the `then` and `else` branches").withPos(srcPos))
      // TODO: tighten the screws of if typing
      // We probably need to find a union type
      Term.If(cond1, thenBranch1, elseBranch1).withPos(srcPos).withTpe(finalTpe).withCVFrom(cond1, thenBranch1, elseBranch1)

  def checkInfix(op: Syntax.InfixOp, lhs: Syntax.Term, rhs: Syntax.Term, expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    op match
      case Syntax.InfixOp.Plus =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Add
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Add
          case _ => PrimitiveOp.I32Add
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Minus =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Sub
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Sub
          case _ => PrimitiveOp.I32Sub
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Mul =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Mul
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Mul
          case _ => PrimitiveOp.I32Mul
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Div =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Div
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Div
          case _ => PrimitiveOp.I32Div
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Mod =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Rem
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Rem
          case _ => PrimitiveOp.I32Rem
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Eq =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Eq
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Eq
            case Type.Base(BaseType.BoolType) => PrimitiveOp.BoolEq
            case _ => PrimitiveOp.I32Eq
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.Neq =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Neq
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Neq
            case Type.Base(BaseType.BoolType) => PrimitiveOp.BoolNeq
            case _ => PrimitiveOp.I32Neq
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.Lt =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Lt
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Lt
            case _ => PrimitiveOp.I32Lt
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.Gt =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Gt
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Gt
            case _ => PrimitiveOp.I32Gt
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.Lte =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Lte
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Lte
            case _ => PrimitiveOp.I32Lte
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.Gte =>
        hopefully:
          val lhs1 = checkTerm(lhs).!!
          val tpe = lhs1.tpe
          val primOp = tpe match
            case Type.Base(BaseType.I32) => PrimitiveOp.I32Gte
            case Type.Base(BaseType.I64) => PrimitiveOp.I64Gte
            case _ => PrimitiveOp.I32Gte
          checkPrimOp(primOp, List(lhs, rhs), expected, srcPos).!!
      case Syntax.InfixOp.And =>
        val primOp = PrimitiveOp.BoolAnd
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case Syntax.InfixOp.Or =>
        val primOp = PrimitiveOp.BoolOr
        checkPrimOp(primOp, List(lhs, rhs), expected, srcPos)
      case op =>
        Left(List(TypeError.GeneralError(s"Unsupported infix operation: $op").withPos(srcPos)))

  def checkPrefix(op: Syntax.PrefixOp, term: Syntax.Term, expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    op match
      case Syntax.PrefixOp.Neg =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Neg
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Neg
          case _ => PrimitiveOp.I32Neg
        checkPrimOp(primOp, List(term), expected, srcPos)
      case Syntax.PrefixOp.Not =>
        val primOp = PrimitiveOp.BoolNot
        checkPrimOp(primOp, List(term), expected, srcPos)
      // case Syntax.PrefixOp.Return =>
      //   hopefully:
      //     ctx.defReturnType match
      //       case Some(expected) =>
      //         if !expected.exists then
      //           sorry(TypeError.GeneralError("you can only `return` within a `def` with an explicit return type").withPos(srcPos))
      //         val term1 = checkTerm(term, expected = expected).!!
      //         Term.PrimOp(PrimitiveOp.Return, Nil, List(term1)).withPos(srcPos).withTpe(Definitions.nothingType).withCVFrom(term1)
      //       case None => sorry(TypeError.GeneralError("you can only `return` within a `def`").withPos(srcPos))

  def tryConsume(captures: CaptureSet, srcPos: SourcePos)(using Context): Result[CaptureSet] =
    hopefully:
      val consumedSet = captures.qualify(AccessMode.Consume())
      val pks = computePeak(consumedSet)
      pks.elems.foreach: cref =>
        cref.core match
          case CaptureRef.CapInst(capId, kind, fromInst) =>
            kind match
              case _: CapKind.Fresh => // ok
              case _ => sorry(TypeError.GeneralError(s"Cannot consume ${cref.show}").withPos(srcPos))
          case _ => sorry(TypeError.GeneralError(s"Cannot consume ${cref.show}").withPos(srcPos))
      consumedSet

  def getConsumedPeaks(cv: CaptureSet)(using Context): Set[CaptureRef.CapInst] =
    val pks = computePeak(cv)
    val pkElems = cv.elems.flatMap: cref =>
      cref.core match
        case pk: CaptureRef.CapInst if cref.mode == AccessMode.Consume() => pk :: Nil
        case _ => Nil
    pkElems.toSet

  def consumedPeaks(cv: CaptureSet)(using Context): CaptureSet =
    val pkElems = cv.elems.flatMap: cref =>
      val pks = computePeak(CaptureSet(cref :: Nil)).elems
      pks.foreach: pk =>
        pk.maybeWithPosFrom(cref)
      pks
    val pkElems1 = pkElems.filter: cref =>
      cref match
        case QualifiedRef(AccessMode.Consume(), core) => true
        case _ => false
    CaptureSet(pkElems1)

  /** Check a function apply.
   * Returns the arguments, the result type, and the consumed capabilities in this apply.
   */
  def checkFunctionApply(funType: Type, args: List[Syntax.Term], srcPos: SourcePos, isDependent: Boolean = true)(using Context): Result[(List[Term], Type, CaptureSet)] =
    //println(s"checkFunctionApply($funType, $args, $isDependent)")
    hopefully:
      funType.simplify match
        case Type.TermArrow(formals, resultType) =>
          if args.length != formals.length then
            sorry(TypeError.GeneralError(s"Argument number mismatch, expected ${formals.length}, but got ${args.length}").withPos(srcPos))
          def go(xs: List[(Syntax.Term, TermBinder)], acc: List[Term], captureSetAcc: List[(Boolean, CaptureSet, EntityWithProvenance)]): (List[Term], Type, List[(Boolean, CaptureSet, EntityWithProvenance)]) = xs match
            case Nil =>
              val args = acc.reverse
              (args, substitute(resultType, args), captureSetAcc)
            case (arg, formal) :: xs => 
              val formalType =
                if isDependent then
                  substitute(formal.tpe, acc.reverse, isParamType = true)
                else formal.tpe
              val tm = UniversalConversion()
              val expectedType = tm.apply(formalType)
              val localSets = tm.createdUniversals
              //println(s"checkArg $arg, expected = $formal1 (from $formal)")
              val arg1 = checkTerm(arg, expected = expectedType).!!
              val css = localSets.map(_.solve()).map: cs =>
                (formal.isConsume, cs, EntityWithProvenance(cs.show, arg.pos, "this argument"))
              go(xs, arg1 :: acc, css ++ captureSetAcc)
          val (args1, outType, css) = go(args `zip` (formals), Nil, Nil)
          // perform separation check
          val sigCaptures = funType.signatureCaptureSet
          val sigCapturesWithDesc = (sigCaptures, EntityWithProvenance(sigCaptures.show, srcPos, "the function's context"))
          val css1 = (css.map(x => (x._2, x._3))) ++ (sigCapturesWithDesc :: Nil)
          for i <- 0 until css1.length do
            for j <- i + 1 until css1.length do
              val (cs1, hint1) = css1(i)
              val (cs2, hint2) = css1(j)
              if !checkSeparation(cs1, cs2) then
                sorry(TypeError.SeparationError(hint1, hint2).withPos(srcPos))
          val toConsume = css.filter(_._1).map(_._2).reduceLeftOption(_ ++ _).getOrElse(CaptureSet.empty)
          val consumedSet = tryConsume(toConsume, srcPos).!!
          consumedSet.elems.foreach: cref =>
            cref.setPos(srcPos)
          (args1, outType, consumedSet)
        case _ => sorry(TypeError.GeneralError(s"Expected a term function, but got ${funType.show}").withPos(srcPos))

  /** Instantiate fresh capabilities in the result of a function apply. */
  def instantiateFresh(t: Term, fromInst: Option[CaptureRef.CapInst] = None)(using Context): Term =
    val tpe = t.tpe
    val (tpe1, insts) = instantiateCaps(tpe, isFresh = true, fromInst = fromInst)
    val t1 = t.withTpe(tpe1)
    t1.meta.put(PeakCreated, insts.toSet)
    t1

  def maybeUnbox(t: Term)(using Context): Result[Term] =
    hopefully:
      if t.tpe.isBoxedType then
        checkUnbox(t, t.pos).!!
      else t

  def checkApply(fun: Syntax.Term | Term, args: List[Syntax.Term], expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val fun1 = 
        fun match
          case t: Term => maybeUnbox(t).!!
          case t: Syntax.Term => maybeUnbox(checkTerm(t).!!).!!
      val funType = fun1.tpe
      funType.simplify match
        case Type.TermArrow(formals, resultType) =>
          val (args1, outType, consumedSet) = checkFunctionApply(funType, args, srcPos).!!
          val resultTerm = Term.Apply(fun1, args1).withPos(srcPos).withTpe(outType)
          resultTerm.withCVFrom(fun1 :: args1*).withMoreCV(consumedSet)
          fun1 match
            case _: VarRef =>
              // skip, since it will already be marked
            case _ =>
              val cv = fun1.tpe.captureSet.elems.map: cref =>
                cref.copy().withPos(srcPos)
              resultTerm.withMoreCV(CaptureSet(cv))
          resultTerm.meta.put(PeakConsumed, getConsumedPeaks(consumedSet))
          resultTerm
        case PrimArrayType(elemType) =>
          args match
            case arg :: Nil =>
              val arg1 = checkTerm(arg, expected = Definitions.i32Type).!!
              Term.PrimOp(
                PrimitiveOp.ArrayGet,
                targs = Nil,
                args = List(fun1, arg1),
              ).withPos(srcPos).withTpe(elemType).withCVFrom(fun1, arg1)
            case _ => sorry(TypeError.GeneralError(s"Expect exact one array index, but got ${args.length}").withPos(srcPos))
        // case BreakCapabilityType(returnType) =>
        //   args match
        //     case arg :: Nil =>
        //       val arg1 = checkTerm(arg, expected = returnType).!!
        //       Term.Apply(fun1, List(arg1)).withPos(srcPos).withTpe(Definitions.nothingType).withCVFrom(fun1, arg1)
        //     case _ => sorry(TypeError.GeneralError(s"Expect exact one argument, but got ${args.length}").withPos(srcPos))
        case funType =>
          sorry(TypeError.GeneralError(s"Expected a term function, but got ${funType.show}").withPos(fun.pos))

  def checkTypeApply(typeFun: Term, targs: Either[List[Syntax.Type | Syntax.CaptureSet], List[Type | CaptureSet]], srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      typeFun.tpe.stripCaptures match
        case funTpe @ Type.TypeArrow(formals, resultType) => 
          val typeArgs = 
            targs match
              case Left(targs) => checkTypeArgs(targs, formals, srcPos).!!
              case Right(targs) => targs
          val resultType1 = substituteType(resultType, typeArgs)
          val resultTerm = Term.TypeApply(typeFun, typeArgs).withPos(srcPos).withTpe(resultType1)
          resultTerm.withCVFrom(typeFun)
          typeFun match
            case _: VarRef =>
            case _ =>
              resultTerm.withMoreCV(funTpe.captureSet)
          targs match
            case Left(targs) =>
              val captureArgsWithDesc: List[(CaptureSet, EntityWithProvenance)] = (targs `zip` typeArgs).collect:
                case (targ: Syntax.CaptureSet, cs: CaptureSet) => 
                  (cs, EntityWithProvenance(cs.show, targ.pos, "this argument"))
              val signature = funTpe.signatureCaptureSet
              val signatureWithDesc = (signature, EntityWithProvenance(signature.show, typeFun.pos, "the function's context"))
              val todoCaptureSets = signatureWithDesc :: captureArgsWithDesc
              for i <- 0 until todoCaptureSets.length do
                for j <- i + 1 until todoCaptureSets.length do
                  val (cs1, hint1) = todoCaptureSets(i)
                  val (cs2, hint2) = todoCaptureSets(j)
                  if !checkSeparation(cs1, cs2) then
                    sorry(TypeError.SeparationError(hint1, hint2).withPos(srcPos))
            case Right(_) =>
          instantiateFresh(resultTerm)
        case _ => 
          sorry(TypeError.GeneralError(s"Expected a type/capture function, but got ${typeFun.tpe.show}").withPos(typeFun.pos))

  def substitute(tpe: Type, args: List[Term], isParamType: Boolean = false)(using Context): Type =
    val argTypes = args.map: arg =>
      arg.maybeAsSingletonType match
        case Some(tp) => tp.asInstanceOf[Type]
        case None => arg.tpe
    val tm = SubstitutionMap(argTypes, if isParamType then Variance.Contravariant else Variance.Covariant)
    tm.apply(tpe)

  def substituteType(tpe: Type, targs: List[Type | CaptureSet], isParamType: Boolean = false)(using Context): Type =
    val tm = TypeSubstitutionMap(targs, startingVariance = if isParamType then Variance.Contravariant else Variance.Covariant)
    tm.apply(tpe)

  def substituteTypeInCaptureSet(cs: CaptureSet, targs: List[Type | CaptureSet], isParamType: Boolean = false)(using Context): CaptureSet =
    val tm = TypeSubstitutionMap(targs, if isParamType then Variance.Contravariant else Variance.Covariant)
    tm.mapCaptureSet(cs)

  def checkPrimOpArgs(op: PrimitiveOp, args: List[Syntax.Term], formals: List[BaseType], resType: BaseType, pos: SourcePos)(using Context): Result[Term] = 
    def go(args: List[Syntax.Term], formals: List[BaseType], acc: List[Term]): Result[List[Term]] = (args, formals) match
      case (Nil, Nil) => Right(acc.reverse)
      case (arg :: args, formal :: formals) =>
        checkTerm(arg, expected = Type.Base(formal).withKind(TypeKind.Star)).flatMap: arg1 =>
          go(args, formals, arg1 :: acc)
      case _ => Left(List(TypeError.GeneralError(s"Argument number mismatch for primitive operation, expected ${formals.length}, but got ${args.length}").withPos(pos)))
    go(args, formals, Nil).map: args1 =>
      Term.PrimOp(op, Nil, args1).withPos(pos).withTpe(Type.Base(resType).withKind(TypeKind.Star)).withCVFrom(args1*)

  def checkUnsafeAsPure(args: List[Syntax.Term], pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      args match
        case arg :: Nil =>
          val arg1 = checkTerm(arg).!!
          val outType = arg1.tpe.stripCaptures
          Term.PrimOp(PrimitiveOp.UnsafeAsPure, Nil, List(arg1)).withPos(pos).withTpe(outType).withCVFrom(arg1)
        case _ => sorry(TypeError.GeneralError(s"Expected one argument, but got ${args.length}").withPos(pos))

  def checkUnbox(arg: Syntax.Term | Term, pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val arg1 = arg match
        case t: Term => t
        case t: Syntax.Term => checkTerm(t).!!
      arg1.tpe.dealiasTypeVar match
        case Type.Boxed(core) =>
          val outType = core
          val cv = core.captureSet
          Term.PrimOp(PrimitiveOp.Unbox, Nil, List(arg1)).withPos(pos).withTpe(outType).withCV(cv)
        case _ => sorry(TypeError.GeneralError(s"Expected a boxed type, but got ${arg1.tpe.show}").withPos(pos))

  def checkBox(arg: Syntax.Term | Term, pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val arg1 = arg match
        case t: Term => t
        case t: Syntax.Term => checkTerm(t).!!
      if arg1.tpe.isPure then arg1
      else
        val outCV = 
          if isStablePath(arg1) then
            CaptureSet.empty
          else arg1.cv
        val outType = Type.Boxed(arg1.tpe)
        Term.PrimOp(PrimitiveOp.Box, Nil, List(arg1)).withPos(pos).withTpe(outType).withCV(outCV)

  def checkPrimOp(op: PrimitiveOp, args: List[Syntax.Term], expected: Type, pos: SourcePos)(using Context): Result[Term] =
    op match
      case PrimitiveOp.I32Add => checkPrimOpArgs(PrimitiveOp.I32Add, args, List(BaseType.I32, BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I32Mul => checkPrimOpArgs(PrimitiveOp.I32Mul, args, List(BaseType.I32, BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I64Add => checkPrimOpArgs(PrimitiveOp.I64Add, args, List(BaseType.I64, BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.I64Mul => checkPrimOpArgs(PrimitiveOp.I64Mul, args, List(BaseType.I64, BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.I32Println => checkPrimOpArgs(PrimitiveOp.I32Println, args, List(BaseType.I32), BaseType.UnitType, pos)
      case PrimitiveOp.I32Read => checkPrimOpArgs(PrimitiveOp.I32Read, args, List(), BaseType.I32, pos)
      case PrimitiveOp.I32Sub => checkPrimOpArgs(PrimitiveOp.I32Sub, args, List(BaseType.I32, BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I32Div => checkPrimOpArgs(PrimitiveOp.I32Div, args, List(BaseType.I32, BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I32Rem => checkPrimOpArgs(PrimitiveOp.I32Rem, args, List(BaseType.I32, BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I64Sub => checkPrimOpArgs(PrimitiveOp.I64Sub, args, List(BaseType.I64, BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.I64Div => checkPrimOpArgs(PrimitiveOp.I64Div, args, List(BaseType.I64, BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.I64Rem => checkPrimOpArgs(PrimitiveOp.I64Rem, args, List(BaseType.I64, BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.I32Eq => checkPrimOpArgs(PrimitiveOp.I32Eq, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I32Neq => checkPrimOpArgs(PrimitiveOp.I32Neq, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I32Lt => checkPrimOpArgs(PrimitiveOp.I32Lt, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I32Gt => checkPrimOpArgs(PrimitiveOp.I32Gt, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I32Lte => checkPrimOpArgs(PrimitiveOp.I32Lte, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I32Gte => checkPrimOpArgs(PrimitiveOp.I32Gte, args, List(BaseType.I32, BaseType.I32), BaseType.BoolType, pos)
      case PrimitiveOp.I64Eq => checkPrimOpArgs(PrimitiveOp.I64Eq, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.I64Neq => checkPrimOpArgs(PrimitiveOp.I64Neq, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.I64Lt => checkPrimOpArgs(PrimitiveOp.I64Lt, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.I64Gt => checkPrimOpArgs(PrimitiveOp.I64Gt, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.I64Lte => checkPrimOpArgs(PrimitiveOp.I64Lte, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.I64Gte => checkPrimOpArgs(PrimitiveOp.I64Gte, args, List(BaseType.I64, BaseType.I64), BaseType.BoolType, pos)
      case PrimitiveOp.BoolEq => checkPrimOpArgs(PrimitiveOp.BoolEq, args, List(BaseType.BoolType, BaseType.BoolType), BaseType.BoolType, pos)
      case PrimitiveOp.BoolNeq => checkPrimOpArgs(PrimitiveOp.BoolNeq, args, List(BaseType.BoolType, BaseType.BoolType), BaseType.BoolType, pos)
      case PrimitiveOp.BoolNot => checkPrimOpArgs(PrimitiveOp.BoolNot, args, List(BaseType.BoolType), BaseType.BoolType, pos)
      case PrimitiveOp.BoolAnd => checkPrimOpArgs(PrimitiveOp.BoolAnd, args, List(BaseType.BoolType, BaseType.BoolType), BaseType.BoolType, pos)
      case PrimitiveOp.BoolOr => checkPrimOpArgs(PrimitiveOp.BoolOr, args, List(BaseType.BoolType, BaseType.BoolType), BaseType.BoolType, pos)
      case PrimitiveOp.I32Neg => checkPrimOpArgs(PrimitiveOp.I32Neg, args, List(BaseType.I32), BaseType.I32, pos)
      case PrimitiveOp.I64Neg => checkPrimOpArgs(PrimitiveOp.I64Neg, args, List(BaseType.I64), BaseType.I64, pos)
      case PrimitiveOp.PutChar => checkPrimOpArgs(PrimitiveOp.PutChar, args, List(BaseType.CharType), BaseType.UnitType, pos)
      case PrimitiveOp.PerfCounter => checkPrimOpArgs(PrimitiveOp.PerfCounter, args, List(), BaseType.I32, pos)
      case PrimitiveOp.UnsafeAsPure => checkUnsafeAsPure(args, pos)
      case PrimitiveOp.Sorry => Right(Term.PrimOp(PrimitiveOp.Sorry, Nil, Nil).withPos(pos).withTpe(Definitions.nothingType).withCV(CaptureSet.empty))
      case PrimitiveOp.Box =>
        hopefully:
          if args.length != 1 then
            sorry(TypeError.GeneralError(s"Argument number mismatch, `box` expects one term argument").withPos(pos))
          checkBox(args.head, pos).!!
      case PrimitiveOp.Unbox =>
        hopefully:
          if args.length != 1 then
            sorry(TypeError.GeneralError(s"Argument number mismatch, `unbox` expects one term argument").withPos(pos))
          checkUnbox(args.head, pos).!!
      case PrimitiveOp.ArrayNew =>
        hopefully:
          given ctx1: Context = ctx.newInferenceScope
          val tv = Inference.createTypeVar("elemType", pos)
          val resultTerm = checkArrayInit(tv, args, pos).!!
          Inference.solveTypeVars()
          resultTerm
      // case PrimitiveOp.Boundary =>
      //   hopefully:
      //     given ctx1: Context = ctx.newInferenceScope
      //     val returnType = Inference.createTypeVar()
      //     if args.length != 1 then
      //       sorry(TypeError.GeneralError(s"Argument number mismatch, `boundary` expects one term argument").withPos(pos))
      //     val arg = args.head
      //     val resultTerm = checkBoundary(returnType, arg, pos).!!
      //     Inference.solveTypeVars()
      //     resultTerm
      case _ => assert(false, s"Unsupported primitive operation: $op")

  def checkArrayInit(elemType: Type, args: List[Syntax.Term], pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val arrConsFunction = Type.TermArrow(
        List(
          TermBinder("size", Definitions.i32Type, isConsume = false),
          TermBinder("elemType", elemType, isConsume = false), 
        ),
        Type.Capturing(Definitions.arrayType(elemType), isReadOnly = false, CaptureSet.universal)
      )
      val (args1, outType, consumedSet) = checkFunctionApply(arrConsFunction, args, pos, isDependent = false).!!
      Term.PrimOp(PrimitiveOp.ArrayNew, elemType :: Nil, args1).withPos(pos).withTpe(outType).withCVFrom(args1*)

  /** `boundary` and `break` have been dropped due to lack of support in the backend
   * But we keep the code here for future reference
   */
  // def checkBoundary(returnType: Type, runner: Syntax.Term, srcPos: SourcePos)(using Context): Result[Term] =
  //   hopefully:
  //     val breakCapType = Definitions.breakCapabilityType(returnType)
  //     val expected = Type.TermArrow(List(TermBinder("_", breakCapType, isConsume = false)), returnType)
  //     val runner1 = checkTerm(runner, expected = expected).!!
  //     val result = Term.PrimOp(PrimitiveOp.Boundary, Nil, List(runner1))
  //     result.withPos(srcPos).withTpe(returnType).withCVFrom(runner1)

  def checkArena(returnType: Type, runner: Syntax.Term, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val regionType = Type.Capturing(Definitions.regionType, isReadOnly = false, CaptureSet.universal)
      val binders = List[TermBinder](TermBinder("reg", regionType, isConsume = false))
      val expected = Type.TermArrow(binders, returnType)
      val expandedRunner =
        if runner.isInstanceOf[Syntax.Term.Lambda] then runner
        else
          Synthetics.etaExpand(runner, binders)
      val runner1 = checkTerm(expandedRunner, expected = expected).!!
      val result = Term.PrimOp(PrimitiveOp.Arena, List(returnType), List(runner1))
      result.withPos(srcPos).withTpe(returnType).withCVFrom(runner1)

  def checkPolyPrimOp(op: PrimitiveOp, targs: List[Syntax.Type | Syntax.CaptureSet], args: List[Syntax.Term], expected: Type, pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      op match
        case PrimitiveOp.ArrayNew =>
          (targs, args) match
            case ((elemType : Syntax.Type) :: Nil, arg1 :: arg2 :: Nil) =>
              val elemType1 = checkType(elemType).!!
              checkArrayInit(elemType1, args, pos).!!
            case _ => sorry(TypeError.GeneralError(s"Argument number mismatch, `newArray` expects one type argument and two term arguments"))
        case PrimitiveOp.Arena =>
          (targs, args) match
            case ((returnType : Syntax.Type) :: Nil, runner :: Nil) =>
              val returnType1 = checkType(returnType).!!
              checkArena(returnType1, runner, pos).!!
            case _ => sorry(TypeError.GeneralError(s"Argument number mismatch, `arena` expects one type argument and one term argument"))
        // case PrimitiveOp.Boundary =>
        //   (targs, args) match
        //     case ((returnType : Syntax.Type) :: Nil, runner :: Nil) =>
        //       val returnType1 = checkType(returnType).!!
        //       checkBoundary(returnType1, runner, pos).!!
        //     case _ => sorry(TypeError.GeneralError(s"Argument number mismatch, `boundary` expects one type argument and one term argument"))
        case _ => sorry(TypeError.GeneralError(s"Primitive operation $op cannot be applied to type arguments").withPos(pos))

  def getVariance(v: Int): Variance =
    if v == 0 then Variance.Invariant
    else if v > 0 then Variance.Covariant
    else Variance.Contravariant

  def checkEnumDef(d: Syntax.Definition.EnumDef, symbol: EnumSymbol)(using Context): Result[Unit] =
    hopefully:
      val typeParams = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
      val variances = d.targs.map(_.variance).map(getVariance)
      (d.variants `zip` symbol.info.variants).foreach: (caseDef, vSym) =>
        val info = checkStructFields(typeParams, variances, caseDef.fields, caseDef.pos).!!
        val info1 = info.copy(enumSymbol = Some(symbol))
        vSym.info = info1

  def checkStructFields(
    typeParams: List[TypeBinder | CaptureBinder], 
    variances: List[Variance], 
    fields: List[Syntax.FieldDef],
    srcPos: SourcePos
  )(using Context): Result[StructInfo] =
    hopefully:
      val ctx1 = ctx.extend(typeParams)
      val fieldNames = fields.map(_.name)
      if fieldNames.distinct.length != fieldNames.length then
        sorry(TypeError.GeneralError("Duplicated field name").withPos(srcPos))
      val fieldInfos = fields.map: fieldDef =>
        val fieldType = checkType(fieldDef.tpe)(using ctx1).!!
        FieldInfo(fieldDef.name, fieldType, fieldDef.isVar)
      val ctxVariances = variances.reverse
      val ctxTArgs = typeParams.reverse
      fieldInfos.foreach: field =>
        val startingVariance = if field.mutable then Variance.Invariant else Variance.Covariant
        val checker = CheckVariance(ctxVariances, startingVariance)
        checker.apply(field.tpe)
        checker.mismatches.foreach:
          case CheckVariance.Mismatch(idx, used) =>
            val targ = ctxTArgs(idx)
            def mutableStr = if field.mutable then "mutable " else ""
            sorry:
              TypeError.GeneralError(
                s"Type parameter defined to be ${showVariance(ctxVariances(idx))} but used as ${showVariance(used)} in ${mutableStr}field ${field.name} of type ${field.tpe.show(using ctx1)}"
              ).withPos(targ.pos)
      StructInfo(typeParams, variances, fieldInfos)

  def checkStructDef(d: Syntax.Definition.StructDef)(using Context): Result[StructInfo] =
    hopefully:
      val binders = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
      val variances = d.targs.map(_.variance).map(getVariance)
      checkStructFields(binders, variances, d.fields, d.pos).!!

  def checkTypeDef(d: Syntax.Definition.TypeDef)(using Context): Result[TypeDefInfo] =
    hopefully:
      val typeBinders = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
      val variances = d.targs.map(_.variance).map(getVariance)
      val ctx1 = ctx.extend(typeBinders)
      val body1 = checkType(d.body)(using ctx1).!!
      if !body1.isPure then
        sorry(TypeError.GeneralError("Rhs of a type definition must be a pure type").withPos(d.body.pos))
      // Check variances
      val ctxVariances = variances.reverse
      val ctxTypeArgs = d.targs.reverse
      val checker = CheckVariance(ctxVariances)
      checker.apply(body1)
      checker.mismatches.foreach:
        case CheckVariance.Mismatch(idx, used) =>
          val srcPos = ctxTypeArgs(idx).pos
          sorry:
            TypeError.GeneralError(
              s"Type parameter defined to be ${showVariance(ctxVariances(idx))} but used as ${showVariance(used)} in type ${body1.show(using ctx1)}"
            ).withPos(srcPos)
      TypeDefInfo(typeBinders, variances, body1)

  def checkDef(d: Syntax.ValueDef)(using Context): Result[(TermBinder, Term)] = d match
    case Syntax.Definition.ValDef(name, tpe, expr) =>
      hopefully:
        val expected1 = tpe match
          case None => Type.NoType()
          case Some(expected) => checkType(expected).!!
        val expr1 = checkTerm(expr, expected = expected1).!!
        val binderType = if expected1.exists then expected1 else expr1.tpe
        val bd = TermBinder(name, binderType, isConsume = false).withPos(d.pos)
        (bd.asInstanceOf[TermBinder], expr1)
    case Syntax.Definition.DefDef(name, _, paramss, resultType, expr) => 
      hopefully:
        def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList], isInitClause: Boolean = false)(using Context): Result[Term] = pss match
          case Nil => 
            val expectedBodyType = resultType match
              case None => Type.NoType()
              case Some(expected) => checkType(expected).!!
            val tm = UniversalConversion()
            val expected1 = tm.apply(expectedBodyType)
            val outTerm = checkTerm(expr, expected = expected1)  //(using ctx.withDefReturnType(expectedBodyType)) // `return` is dropped, but keep it for now
            val localSets = tm.createdUniversals
            val css = localSets.map(_.solve())
            // Perform separation checking
            for i <- 0 until css.size do
              for j <- i + 1 until css.size do
                val ci = css(i)
                val cj = css(j)
                if !checkSeparation(ci, cj) then
                  sorry(TypeError.GeneralError(s"In the returned value of this method, ${ci.show} and ${cj.show} consumes the same capability twice").withPos(resultType.get.pos))
            // Check the level of fresh caps
            val curLevel = ctx.freshLevel
            var consumedPks = Set.empty[CaptureRef.CapInst]
            css.foreach: cs =>
              val pks = computePeak(cs)
              pks.elems.foreach: cref =>
                cref.core match
                  case inst @ CaptureRef.CapInst(capId, CapKind.Fresh(level), fromInst) =>
                    if level < curLevel then
                      sorry(TypeError.GeneralError(s"Treating capabilities ${cs.show} as fresh in the result type but they come from the outside").withPos(expr.pos))
                    else if fromInst.isDefined then
                      sorry(TypeError.GeneralError(
                        s"Treating capabilities ${cs.show} as fresh in the result type but they depends on the root of id ${fromInst.get}").withPos(expr.pos))
                    else consumedPks = consumedPks + inst
                  case CaptureRef.CapInst(capId, CapKind.Sep(level), fromInst) =>
                    sorry(TypeError.GeneralError(s"Treating capabilities ${cs.show} as fresh in the result type but they are not fresh").withPos(expr.pos))
                  case _ =>
            outTerm.foreach: t =>
              if expectedBodyType.exists then
                t.setTpe(expectedBodyType)
              t.meta.put(PeakConsumed, consumedPks)
            outTerm
          case (p @ Syntax.TermParamList(params)) :: pss => 
            val params1 = checkTermParamList(params, srcPos = p.pos).!!
            def checkBody(using Context): Result[Term] = go(pss)
            checkTermAbstraction(params1, checkBody, srcPos = d.pos)
          case Syntax.TypeParamList(params) :: pss =>
            val params1 = checkTypeParamList(params).!!
            val maybeTypeBinder = params1.find:
              case binder: TypeBinder => true
              case _ => false
            maybeTypeBinder match
              case Some(binder) if !isInitClause =>
                sorry(TypeError.GeneralError("Type parameters are only allowed in the first parameter clause").withPos(binder.pos))
              case _ =>
            def checkBody(using Context): Result[Term] = go(pss)
            checkTypeAbstraction(params1, checkBody, srcPos = d.pos)
        val pss1: List[Syntax.TermParamList | Syntax.TypeParamList] = paramss match
          case Nil => List(Syntax.TypeParamList(Nil))
          case pss => pss
        val expr1 = go(pss1, isInitClause = true).!!
        val bd = TermBinder(name, expr1.tpe, isConsume = false).withPos(d.pos)
        (bd.asInstanceOf[TermBinder], expr1)

  def isTypePolymorphicParamList(ps: Syntax.TermParamList | Syntax.TypeParamList)(using Context): Boolean = ps match
    case Syntax.TermParamList(_) => false
    case Syntax.TypeParamList(params) => params.exists:
      case Syntax.TypeParam(_, _) => true
      case _ => false

  def splitExtensionParamLists(
    pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): (List[Syntax.TypeParamList], List[Syntax.TypeParamList | Syntax.TermParamList]) =
    pss match
      case Nil => (Nil, pss)
      case p :: pss =>
        if isTypePolymorphicParamList(p) then
          (p.asInstanceOf[Syntax.TypeParamList] :: Nil, pss)
        else
          (Nil, p :: pss)

  def checkExtensionDef(d: Syntax.Definition.ExtensionDef, sym: ExtensionSymbol)(using Context): Result[ExtensionInfo] =
    hopefully:
      val typeArgs = sym.info.typeParams
      val ctx1 = ctx.extend(typeArgs)
      val selfArgBinder = checkTermParam(d.selfArg)(using ctx1).!!
      val methodInfos = d.methods.map: method =>
        val (typeParamss, restParamss) = splitExtensionParamLists(method.paramss)
        val method1 =
          method.copy(paramss = typeParamss ++ (Syntax.TermParamList(List(d.selfArg)).withPosFrom(method) :: restParamss)).withPosFrom(method)
        val (bd, expr) = checkDef(method1)(using ctx1).!!
        ExtensionMethod(method.name, bd.tpe, expr)
      ExtensionInfo(typeArgs, selfArgBinder.tpe, methodInfos)

  def extractDefType(d: Syntax.Definition.DefDef, requireExplictType: Boolean = true)(using Context): Result[Type] = 
    def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[Type] = pss match
      case Nil => 
        d.resultType match
          case None => 
            if requireExplictType then Left(List(TypeError.GeneralError("Explicit type annotation is required for top-level definitions").withPos(d.pos)))
            else Right(Definitions.anyType)
          case Some(resultType) => checkType(resultType)
      case (ps: Syntax.TermParamList) :: pss =>
        checkTermParamList(ps.params, srcPos = ps.pos).flatMap: params =>
          go(pss)(using ctx.extend(params)).flatMap: resultType1 =>
            Right(Type.TermArrow(params, resultType1).withKind(TypeKind.Star))
      case (ps: Syntax.TypeParamList) :: pss =>
        checkTypeParamList(ps.params).flatMap: params =>
          go(pss)(using ctx.extend(params)).flatMap: resultType1 =>
            Right(Type.TypeArrow(params, resultType1).withKind(TypeKind.Star))
    hopefully:
      val res = go(d.paramss).!!
      if d.paramss.isEmpty then
        Type.TypeArrow(Nil, res)
      else res

  def extractValType(d: Syntax.Definition.ValDef)(using Context): Result[Type] = 
    d.tpe match
      case None => Left(List(TypeError.GeneralError("Explicit type annotation is required for top-level definitions").withPos(d.pos)))
      case Some(expected) => checkType(expected)

  def extractDefnType(d: Syntax.ValueDef)(using Context): Result[Type] = d match
    case d: Syntax.Definition.ValDef => extractValType(d)
    case d: Syntax.Definition.DefDef => extractDefType(d)

  def createSymbol(d: Syntax.Definition, m: Module)(using Context): Result[Symbol] =
    hopefully:
      d match
        case d: (Syntax.Definition.DefDef | Syntax.Definition.ValDef) =>
          DefSymbol(d.name, Definitions.anyType, m).withPosFrom(d)
        case d: Syntax.Definition.StructDef =>
          val tparams = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
          val variances = d.targs.map(_.variance).map(getVariance)
          val info = StructInfo(tparams, variances, Nil)
          StructSymbol(d.name, info, m).withPosFrom(d)
        case d: Syntax.Definition.ExtensionDef =>
          val tparams = checkTypeParamList(d.typeArgs).!!
          val info = ExtensionInfo(tparams, Definitions.anyType, Nil)
          ExtensionSymbol(d.name, info, m).withPosFrom(d)
        case d: Syntax.Definition.TypeDef =>
          val tparams = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
          val variances = d.targs.map(_.variance).map(getVariance)
          val info = TypeDefInfo(tparams, variances, Type.NoType())
          TypeDefSymbol(d.name, info, m).withPosFrom(d)
        case d: Syntax.Definition.EnumDef =>
          val tparams = checkTypeParamList(d.targs.map(_.toTypeParam)).!!
          val variances = d.targs.map(_.variance).map(getVariance)
          val enumSymbol = EnumSymbol(d.name, null, m).withPosFrom(d)
          val variantSymbols = d.variants.map: caseDef =>
            StructSymbol(caseDef.name, StructInfo(tparams, variances, Nil, Some(enumSymbol)), m)
          val info = EnumInfo(tparams, variances, variantSymbols)
          enumSymbol.info = info
          enumSymbol

  /** Elaborate symbols with their declared types. */
  def elaborateType(d: Syntax.Definition, sym: Symbol, allSyms: List[Symbol])(using Context): Result[Unit] =
    val ctx1 = ctx.addSymbols(allSyms)
    hopefully:
      (d, sym) match
        case (d: Syntax.Definition.ValDef, sym: DefSymbol) =>
          val defType = (extractDefnType(d)(using ctx1) || Right(Definitions.anyType)).!!
          sym.tpe = defType
        case (d: Syntax.Definition.DefDef, sym: DefSymbol) =>
          val defType = extractDefType(d)(using ctx1).!!
          sym.tpe = defType
        case (d: Syntax.Definition.ExtensionDef, sym: ExtensionSymbol) =>
          val typeArgs = sym.info.typeParams
          val ctx2 = ctx1.extend(typeArgs)
          val selfArgBinder = checkTermParam(d.selfArg)(using ctx2).!!
          val methodInfos = d.methods.map: method =>
            val (typeParamss, restParamss) = splitExtensionParamLists(method.paramss)
            val method1 =
              method.copy(paramss = typeParamss ++ (Syntax.TermParamList(List(d.selfArg)).withPosFrom(method) :: restParamss)).withPosFrom(method)
            val methodType = extractDefnType(method1)(using ctx2).!!
            ExtensionMethod(method.name, methodType, body = null)
          sym.info = ExtensionInfo(typeArgs, selfArgBinder.tpe, methodInfos)
        case _ => // do nothing

  def mergeResults[A](mxs: List[Result[A]]): Result[List[A]] =
    var errors: List[TypeError] = Nil
    var results: List[A] = Nil
    for mx <- mxs do mx match
      case Left(es1) => errors = errors ++ es1
      case Right(x) => results = results :+ x
    if errors.isEmpty then
      Right(results)
    else
      Left(errors)

  def checkModules(modules: List[Syntax.Module])(using Context): Result[List[Module]] =
    hopefully:
      // Create all modules
      val mods: List[Expr.Module] = modules.map: m =>
        Expr.Module(getModuleName(m.name), defns = Nil, parent = null)
      // Create symbols with infos as placeholders
      val modSyms: List[List[Symbol]] = (modules `zip` mods).map: (m0, m) =>
        m0.defs.map: d =>
          createSymbol(d, m).!!
      val allSyms: List[Symbol] = modSyms.flatten
      // Elaborate symbol types
      (modules `zip` modSyms).foreach: (m0, syms) =>
        (m0.defs `zip` syms).foreach: (d, sym) =>
          elaborateType(d, sym, allSyms).!!
      // Typecheck type-related definitions in each module
      var ctxWithAllSyms = ctx.addSymbols(allSyms)
      (modules `zip` mods `zip` modSyms).foreach: 
        case ((m0, m), syms) =>
          // Typecheck struct and type definitions
          val typeDefResults: List[Result[Definition]] = (m0.defs `zip` syms).collect:
            case (d: Syntax.Definition.StructDef, sym: StructSymbol) => 
              hopefully:
                val info1 = checkStructDef(d)(using ctxWithAllSyms).!!
                sym.info = info1
                Definition.StructDef(sym).withPosFrom(d)
            case (d: Syntax.Definition.TypeDef, sym: TypeDefSymbol) =>
              hopefully:
                val info1 = checkTypeDef(d)(using ctxWithAllSyms).!!
                sym.info = info1
                Definition.TypeDef(sym).withPosFrom(d)
            case (d: Syntax.Definition.EnumDef, sym: EnumSymbol) =>
              hopefully:
                checkEnumDef(d, sym)(using ctxWithAllSyms).!!
                Definition.EnumDef(sym).withPosFrom(d)
          // Done
          val typeDefs = mergeResults(typeDefResults).!!
          m.defns = typeDefs
      // Typecheck the rest of the definitions in each module
      (modules `zip` mods `zip` modSyms).foreach: 
        case ((m0, m), syms) =>
          // Typecheck extension definitions
          val extensionDefsResults: List[Result[Definition]] = (m0.defs `zip` syms).collect:
            case (d: Syntax.Definition.ExtensionDef, sym: ExtensionSymbol) =>
              hopefully:
                val info1 = checkExtensionDef(d, sym)(using ctxWithAllSyms).!!
                sym.info = info1
                Definition.ExtensionDef(sym).withPosFrom(d)
          // Typecheck value definitions, i.e. `val` and `def`
          val valueDefsResults: List[Result[Definition]] = (m0.defs `zip` syms).collect:
            case (d: Syntax.Definition.ValDef, sym: DefSymbol) =>
              hopefully:
                val (bd, expr) = checkDef(d)(using ctxWithAllSyms).!!
                if !d.tpe.isDefined then
                  sym.tpe = expr.tpe
                Definition.ValDef(sym, expr).withPosFrom(d)
            case (d: Syntax.Definition.DefDef, sym: DefSymbol) =>
              hopefully:
                val (bd, expr) = checkDef(d)(using ctxWithAllSyms).!!
                Definition.ValDef(sym, expr).withPosFrom(d)
          val moreDefs = mergeResults(extensionDefsResults ++ valueDefsResults).!!
          // Done
          val allDefs = m.defns ++ moreDefs
          m.defns = allDefs
      // Return all typechecked modules
      mods

  def getModuleName(name: Syntax.ModuleName): String =
    name match
      case Syntax.ModuleName.Root() => "<root>"
      case Syntax.ModuleName.Qualified(Syntax.ModuleName.Root(), name) => name
      case _ => assert(false, "Qualified module names are not yet supported")

  def isStablePath(t: Term): Boolean = t match
    case Term.Select(base, fieldInfo) => isStablePath(base) && !fieldInfo.mutable
    case Term.SymbolRef(_) => true
    case Term.BinderRef(_) => true
    case _ => false

  /** Search for extention method who has a field of `field` and is applicable to `baseType` */
  def searchExtension(baseType: Type, field: String, srcPos: SourcePos)(using Context): Option[(ExtensionSymbol, List[(Type | CaptureSet)])] = boundary:
    ctx.symbols.foreach:
      case sym: ExtensionSymbol if sym.info.methods.exists(_.name == field) =>
        given ctx1: Context = ctx.newInferenceScope
        val typeArgs: List[Type | CaptureSet] = sym.info.typeParams.map:
          case TypeBinder(name, bound) => Inference.createTypeVar(name, srcPos, bound)
          case CaptureBinder(name, bound) => 
            CaptureSet.empty // for now, capture set inference is not supported for extension search
        val selfArgType = substituteType(sym.info.selfArgType, typeArgs, isParamType = true)
        val tm = UniversalConversion()
        val formal = tm.apply(selfArgType)
        if TypeComparer.checkSubtype(baseType, formal) then
          solveTypeVars()
          break(Some((sym, typeArgs)))
      case _ =>
    None

  // def checkStablePath(t: Syntax.Term.Select)(using Context): Result[Term] =
  //   hopefully:
  //     def constructPath(base: Term, field: String): Term =
  //       getFieldInfo(base.tpe, field) match
  //         case Some(fieldInfo) =>
  //           if fieldInfo.mutable then
  //             sorry(TypeError.GeneralError(s"Field is mutable, so this is not a stable path").withPosFrom(t))
  //           val outTerm = Term.Select(base, fieldInfo).withPosFrom(t).withTpe(fieldInfo.tpe)
  //           val singletonSet = outTerm.asSingletonType.singletonCaptureSet
  //           outTerm.withCV(singletonSet)
  //         case None => sorry(TypeError.GeneralError(s"Field ${t.field} is not found in $base").withPosFrom(t))
  //     def recur(base: Syntax.Term): Term = base match
  //       case base: Syntax.Term.Ident => checkIdent(base).!!
  //       case base: Syntax.Term.Select => 
  //         val base1 = recur(base.base)
  //         constructPath(base1, base.field)
  //       case _ => sorry(TypeError.GeneralError(s"This is not a stable path").withPosFrom(t))
  //     val base1 = recur(t.base)
  //     val out = constructPath(base1, t.field)
  //     println(s"out.cv = ${out.cv.show}")
  //     println(s"consumedPeaks = ${ctx.consumedPeaks.show}")
  //     if !checkSeparation(out.cv, ctx.consumedPeaks) then
  //       sorry(TypeError.GeneralError(s"This uses a consumed capability").withPosFrom(t))
  //     out

  def checkSelect(base: Syntax.Term, field: String, srcPos: SourcePos, inputTypeArgs: List[Syntax.Type | Syntax.CaptureSet] = Nil)(using Context): Result[Term] =
    hopefully:
      val base1 = checkTerm(base).!!
      def fail: Result[Nothing] = Left(List(TypeError.TypeMismatch(s"a type with the field $field", base1.tpe.show).withPosFrom(base)))
      def tryPrimArray: Result[Term] =
        hopefully:
          base1.tpe.simplify match
            case PrimArrayType(elemType) =>
              field match
                case "size" | "length" =>
                  if !inputTypeArgs.isEmpty then
                    sorry(TypeError.TypeMismatch(s"a type/capture function", "the size of an array").withPos(srcPos))
                  Term.PrimOp(PrimitiveOp.ArrayLen, Nil, List(base1)).withPos(srcPos).withTpe(Definitions.i32Type).withCV(base1.cv)
                case _ => fail.!!
            case _ => fail.!!
      def tryStruct: Result[Term] =
        hopefully:
          getFieldInfo(base1.tpe, field) match
            case Some(fieldInfo) =>
              var outTerm = Term.Select(base1, fieldInfo).withPos(srcPos).withTpe(fieldInfo.tpe).withCV(base1.cv)
              if isStablePath(outTerm) && !outTerm.tpe.isPure then
                val singletonSet = outTerm.asSingletonType.singletonCaptureSet
                val narrowedType = Type.Capturing(outTerm.tpe.stripCaptures, isReadOnly = false, singletonSet)
                outTerm = outTerm.withTpe(narrowedType).withCV(singletonSet)
              if !inputTypeArgs.isEmpty then
                outTerm = checkTypeApply(outTerm, Left(inputTypeArgs), srcPos).!!
              outTerm
            case _ => fail.!!
      def tryExtension: Result[Term] =
        hopefully:
          searchExtension(base1.tpe, field, srcPos) match
            case Some((extSym, typeArgs)) =>
              val method = extSym.info.methods.find(_.name == field).get
              val funType = substituteType(method.tpe, typeArgs, isParamType = false)
              val resolvedTerm = Term.ResolveExtension(extSym, typeArgs, method.name).withPos(srcPos).withTpe(funType).withCV(CaptureSet.empty)
              val maybeTypedApplied = 
                if inputTypeArgs.isEmpty then
                  resolvedTerm
                else checkTypeApply(resolvedTerm, Left(inputTypeArgs), srcPos).!!
              val appliedTerm = checkApply(maybeTypedApplied, List(base), funType, srcPos).map(instantiateFresh(_)).!!
              appliedTerm
            case None => fail.!!
      (tryPrimArray || tryStruct || tryExtension).!!

  def checkAssign(lhs: Syntax.Term, rhs: Syntax.Term, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      def fail = sorry(TypeError.GeneralError(s"Cannot assign to this target").withPosFrom(lhs))
      val lhs1 = checkTerm(lhs).!!
      lhs1 match
        case lhs1 @ Term.Select(base1, fieldInfo) =>
          if fieldInfo.mutable then
            if base1.tpe.isReadOnly then
              sorry(TypeError.TypeMismatch("a mutable reference", base1.tpe.show).withPosFrom(base1))
            val rhs1 = checkTerm(rhs, expected = lhs1.tpe).!!
            Term.PrimOp(PrimitiveOp.StructSet, Nil, List(lhs1, rhs1)).withPos(srcPos).withTpe(Definitions.unitType).withCVFrom(lhs1, rhs1)
          else
            sorry(TypeError.GeneralError(s"Field is not mutable").withPosFrom(lhs))
        case lhs1 @ Term.PrimOp(PrimitiveOp.ArrayGet, Nil, arr :: arg :: Nil) =>
          val PrimArrayType(elemType) = arr.tpe.stripCaptures: @unchecked
          val rhs1 = checkTerm(rhs, expected = elemType).!!
          Term.PrimOp(PrimitiveOp.ArraySet, Nil, List(arr, arg, rhs1)).withPos(srcPos).withTpe(Definitions.unitType).withCVFrom(lhs1, rhs1)
        case _ => 
          fail

  def getFieldInfo(tpe: Type, field: String)(using Context): Option[FieldInfo] = 
    def go(tpe: Type): Option[FieldInfo] =
      tpe.simplify match
        case AppliedStructType(classSym, targs) => 
          classSym.info.fields.find(_.name == field).map: fieldInfo =>
            val fieldType = substituteType(fieldInfo.tpe, targs, isParamType = false)
            FieldInfo(fieldInfo.name, fieldType, fieldInfo.mutable)
        case Type.Capturing(tpe, _, _) => go(tpe)
        case Type.RefinedType(core, refinements) =>
          refinements.find(_.name == field).orElse(go(core))
        case RegionRefType(inner) => go(inner)
        case _ => None
    go(tpe)

  def allFieldNames(tp: Type)(using Context): List[String] = tp match
    // case Type.BinderRef(idx) => allFieldNames(getBinder(idx).asInstanceOf[TypeBinder].bound) // TODO: fix this case
    case Type.Capturing(inner, _, _) => allFieldNames(inner)
    case AppliedStructType(classSym, targs) => 
      val fieldNames = classSym.info.fields.map(_.name)
      fieldNames
    case Type.RefinedType(base, refinements) =>
      allFieldNames(base) ++ refinements.map(_.name)
    case Type.Var(ref) => allFieldNames(ref.tpe)
    case Type.Select(base, fieldInfo) => allFieldNames(fieldInfo.tpe)
    case _ => Nil
