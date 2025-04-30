package cavia
package typechecking

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
import core.*
import ast.*
import reporting.trace

object TypeChecker:
  import Expr.*
  import Syntax.AccessMode
  import Binder.*
  import Inference.*

  /** Type checking context. */
  case class Context(binders: List[Binder], symbols: List[Symbol], inferenceState: InferenceState, consumedPeaks: CaptureSet = CaptureSet.empty, freshLevel: Int = 0):
    /** Extend the context with a list of binders. */
    def extend(bds: List[Binder]): Context =
      if bds.isEmpty then this
      else
        var newBinders = binders
        for bd <- bds do
          newBinders = bd :: newBinders
        copy(binders = newBinders)
    def extend(bd: Binder): Context = extend(bd :: Nil)

    def addSymbol(sym: Symbol): Context =
      copy(symbols = sym :: symbols)

    def addSymbols(syms: List[Symbol]): Context =
      if syms.isEmpty then this
      else
        var newSymbols = symbols
        for sym <- syms do
          newSymbols = sym :: newSymbols
        copy(symbols = newSymbols)

    def newInferenceScope: Context =
      copy(inferenceState = InferenceState.empty)

    def newFreshLevel: Context =
      copy(freshLevel = freshLevel + 1)

    def moreConsumedPeaks(peaks: CaptureSet): Context =
      copy(consumedPeaks = consumedPeaks ++ peaks)

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
    case SeparationError(cs1: String, cs2: String)
    case GeneralError(msg: String)

    override def toString(): String = this match
      case UnboundVariable(name, addenda) => s"Unbound variable: $name$addenda"
      case TypeMismatch(expected, actual) => s"Type mismatch: expected $expected, but got $actual"
      case LeakingLocalBinder(tp) => s"Leaking local binder: $tp"
      case SeparationError(cs1, cs2) => s"Separation error: $cs1 and $cs2 are not separated"
      case GeneralError(msg) => msg

  def ctx(using myCtx: Context): Context = myCtx
  
  type Result[+A] = Either[TypeError, A]

  def lookupBinder(name: String)(using ctx: Context): Option[(Binder, Int)] =
    ctx.binders.zipWithIndex.find((binder, _) => binder.name == name).map: (bd, idx) =>
      (bd.shift(idx + 1), idx)

  def lookupSymbol(name: String)(using ctx: Context): Option[Symbol] =
    ctx.symbols.find(_.name == name)

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

  def checkTermParam(param: Syntax.TermParam)(using ctx: Context): Result[TermBinder] =
    checkType(param.tpe).map: tpe =>
      val binder: TermBinder = TermBinder(param.name, tpe, param.isConsume)
      binder.maybeWithPosFrom(param)

  def checkTypeParam(param: Syntax.TypeParam)(using ctx: Context): Result[TypeBinder] =
    val bound: Result[Type] = param.bound match
      case None => Right(Definitions.anyType.withPosFrom(param))
      case Some(tpe) => 
        checkType(tpe).flatMap: tpe1 =>
          val cs = tpe1.captureSet
          if TypeComparer.checkSubcapture(cs, CaptureSet.empty) then
            Right(tpe1)
          else
            Left(TypeError.GeneralError(s"Type parameter ${param.name} has a non-pure bound type: ${tpe1.show}, consider boxing it").withPos(param.pos))
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
      case Some((binder: Binder.TypeBinder, idx)) => Left(TypeError.UnboundVariable(ref.name, s"I found a type name, but was looking for either a term or capture name").withPos(ref.pos))
      case _ => Left(TypeError.UnboundVariable(ref.name).withPos(ref.pos))

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
    case "String" => Some(Definitions.strType)
    case "Any" => Some(Definitions.anyType)
    case "i32" => Some(Definitions.i32Type)
    case "i64" => Some(Definitions.i64Type)
    case "bool" => Some(Definitions.boolType)
    case "array" => Some(Definitions.arrayConstructorType)
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

  def checkTypeArg(targ: (Syntax.Type | Syntax.CaptureSet))(using Context): Result[Type | CaptureSet] = targ match
    case targ: Syntax.Type => checkType(targ)
    case targ: Syntax.CaptureSet => checkCaptureSet(targ)
    
  def checkType(tpe: Syntax.Type)(using Context): Result[Type] = tpe match
    case Syntax.Type.Ident(name) => 
      def tryBaseType: Result[Type] = findBaseType(name) match
        case Some(baseType) => Right(baseType.maybeWithPosFrom(tpe))
        case None => Left(TypeError.UnboundVariable(name).withPos(tpe.pos))
      def tryBinder: Result[Type] = lookupBinder(name) match
        case Some((binder: Binder.TypeBinder, idx)) => Right(Type.BinderRef(idx).withKind(TypeKind.Star).maybeWithPosFrom(tpe))
        case Some((binder: Binder, idx)) => 
          Left(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a type").maybeWithPosFrom(tpe))
        case None => Left(TypeError.UnboundVariable(name).maybeWithPosFrom(tpe))
      def trySymbol: Result[Type] = lookupSymbol(name) match
        case Some(sym: StructSymbol) => 
          val numArgs = sym.info.targs.length
          val kind =
            if numArgs == 0 then TypeKind.Star
            else TypeKind.Arrow(numArgs, TypeKind.Star)
          Right(Type.SymbolRef(sym).withKind(kind).maybeWithPosFrom(tpe))
        case _ => Left(TypeError.UnboundVariable(name).maybeWithPosFrom(tpe))
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
        Type.Capturing(inner1, captureSet1).maybeWithPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.AppliedType(tycon, args) =>
      hopefully:
        val tycon1 = checkType(tycon).!!
        tycon1.kind match
          case TypeKind.Arrow(arity, resKind) =>
            if arity != args.length then
              sorry(TypeError.GeneralError(s"Number of type arguments mismatch: expected $arity, but got ${args.length}").withPos(tpe.pos))
            val targs1: List[Type | CaptureSet] = args.map: arg =>
              arg match
                case arg: Syntax.Type => checkType(arg).!!
                case arg: Syntax.CaptureSet => checkCaptureSet(arg).!!
            Type.AppliedType(tycon1, targs1).maybeWithPosFrom(tpe).withKind(resKind)
          case _ => sorry(TypeError.GeneralError("This is not a type constructor").withPos(tycon.pos))

  def checkTermParamList(params: List[Syntax.TermParam])(using Context): Result[List[TermBinder]] =
    hopefully:
      var result: ArrayBuffer[TermBinder] = ArrayBuffer.empty
      var nowCtx: Context = ctx
      for p <- params do
        val bd = checkTermParam(p)(using nowCtx).!!
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

  private def dropLocalParams(crefs: List[QualifiedRef], numParams: Int): (Boolean, List[QualifiedRef]) = 
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
                val tpe = checkType(t).!!
                if TypeComparer.checkSubtype(tpe, formal1) then
                  tpe
                else
                  sorry(TypeError.GeneralError(s"Type argument ${tpe.show} does not conform to the bound ${f.show}").withPosFrom(t))
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

  def instantiateCaps(tpe: Type, isFresh: Boolean)(using Context): Type =
    val curLevel = ctx.freshLevel
    val capKind = if isFresh then CapKind.Fresh(curLevel) else CapKind.Sep(curLevel)
    val tm1 = CapInstantiation(() => CaptureRef.makeCapInst(capKind, fromInst = None))
    val tpe1 = tm1.apply(tpe)
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
            if tm2.localCaps.isEmpty then
              None
            else
              val info = FieldInfo(fieldName, fieldType, fieldInfo.mutable)
              Some(info)
    if fieldInfos.isEmpty then tpe1
    else tpe1.refined(fieldInfos)

  def instantiateBinderCaps(binder: TermBinder)(using Context): TermBinder =
    val tpe1 = instantiateCaps(binder.tpe, isFresh = binder.isConsume)
    val binder1 = TermBinder(binder.name, tpe1, binder.isConsume).maybeWithPosFrom(binder).asInstanceOf[TermBinder]
    binder1

  def checkStructInit(classSym: StructSymbol, targs: List[Syntax.Type | Syntax.CaptureSet], args: List[Syntax.Term], expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    val tformals = classSym.info.targs
    val fields = classSym.info.fields
    hopefully:
      if targs.length != tformals.length then
        sorry(TypeError.GeneralError(s"Constructor type argument number mismatch, expected ${tformals.length}, but got ${targs.length}").withPos(srcPos))
      if args.length != fields.length then
        sorry(TypeError.GeneralError(s"Constructor argument number mismatch, expected ${fields.length}, but got ${args.length}").withPos(srcPos))
      val typeArgs = checkTypeArgs(targs, tformals, srcPos).!!
      val fields1: List[(String, Type, Boolean)] = fields.map: field =>
        val tpe = field.tpe
        val fieldType = substituteType(tpe, typeArgs, isParamType = true)
        (field.name, fieldType, field.mutable)
      val argFormals = fields1.map(_._2)
      val syntheticFunctionType = Type.TermArrow(argFormals.map(tpe => TermBinder("_", tpe, isConsume = false)), Type.AppliedType(Type.SymbolRef(classSym), typeArgs))
      val (termArgs, _, consumedSet) = checkFunctionApply(syntheticFunctionType, args, srcPos, isDependent = false).!!
      val classType = 
        if typeArgs.isEmpty then
          Type.SymbolRef(classSym)
        else
          Type.AppliedType(Type.SymbolRef(classSym), typeArgs)
      val termCaptureElems = termArgs.flatMap: arg =>
        arg.maybeAsSingletonType match
          case Some(singleton) => QualifiedRef(AccessMode.Normal(), singleton.asCaptureRef) :: Nil
          case None => arg.tpe.captureSet.elems
      val refinements: List[FieldInfo] = (fields1 `zip` termArgs).flatMap: 
        case ((name, fieldType, mutable), arg) =>
          if fieldType.isPure then None
          else Some(FieldInfo(name, arg.tpe, mutable))
      val captureSet = CaptureSet.universal ++ CaptureSet(termCaptureElems)
      val outType = Type.Capturing(classType, captureSet)
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
        val params1 = params.map(instantiateBinderCaps)
        val ctx1 = ctx.extend(params1)
        val body1 = checkBody(using ctx1).!!
        val bodyCV = body1.cv
        if bodyCV.elems.contains(CaptureRef.CAP()) then
          sorry(TypeError.GeneralError("A `cap` that is not nameable is captured by the body of this lambda; try naming `cap`s explicitly with capture parameters").withPos(srcPos))
        val (_, outCV) = dropLocalParams(bodyCV.elems.toList, params.length)
        val outCV1 = dropLocalFreshCaps(outCV, srcPos).!!
        val outTerm = Term.TermLambda(params, body1, skolemizedBinders = params1).withPos(srcPos)
        val outType = Type.Capturing(Type.TermArrow(params, body1.tpe), CaptureSet(outCV1))
        outTerm.withTpe(outType).withCV(CaptureSet.empty)

  def checkTypeAbstraction(params: List[TypeBinder | CaptureBinder], checkBody: Context ?=> Result[Term], srcPos: SourcePos)(using Context): Result[Term] =
    inNewFreshLevel:
      hopefully:
        val ctx1 = ctx.extend(params)
        val body1 = checkBody(using ctx1).!!
        val bodyCV = body1.cv
        if bodyCV.elems.contains(CaptureRef.CAP()) then
          sorry(TypeError.GeneralError("A `cap` that is not nameable is captured by the body of this lambda; try naming `cap`s explicitly with capture parameters").withPos(srcPos))
        val (existsLocalParams, outCV) = dropLocalParams(bodyCV.elems.toList, params.length)
        if existsLocalParams then
          sorry(TypeError.GeneralError("local capture parameters captured by the body of a the lambda"))
        val outCV1 = dropLocalFreshCaps(outCV, srcPos).!!
        val outTerm = Term.TypeLambda(params, body1).withPos(srcPos)
        val outType = Type.Capturing(Type.TypeArrow(params, body1.tpe), CaptureSet(outCV1))
        outTerm.withTpe(outType).withCV(CaptureSet.empty)

  def checkTerm(t: Syntax.Term, expected: Type = Type.NoType())(using Context): Result[Term] = 
    val result: Result[Term] = t match
      case Syntax.Term.Ident(name) => 
        hopefully:
          val (ref, tpe) = lookupAll(name) match
            case Some(sym: DefSymbol) => (Term.SymbolRef(sym): VarRef, sym.tpe)
            case Some((binder: Binder.TermBinder, idx)) => (Term.BinderRef(idx): VarRef, binder.tpe)
            case Some((binder: Binder, idx)) => sorry(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a term").withPos(t.pos))
            case _ => sorry(TypeError.UnboundVariable(name).withPos(t.pos))
          //if !tpe.isPure then markFree(ref)
          val cv = if !tpe.isPure then ref.asSingletonType.singletonCaptureSet else CaptureSet.empty

          if !checkSeparation(cv, ctx.consumedPeaks) then
            sorry(TypeError.GeneralError(s"This uses a consumed capability").withPos(t.pos))

          tpe.stripCaptures match
            case LazyType(resultType) => 
              val t1 = Term.TypeApply(ref, Nil).withPosFrom(t)
              val outTerm = t1.withTpe(resultType).withCV(cv)
              instantiateFresh(outTerm)
            case _ =>
              val tpe1 = 
                if tpe.isPure then 
                  tpe 
                else Type.Capturing(tpe.stripCaptures, ref.asSingletonType.singletonCaptureSet).withKind(TypeKind.Star)
              ref.withPosFrom(t).withTpe(tpe1).withCV(cv)
      case Syntax.Term.StrLit(value) => 
        Right(Term.StrLit(value).withPosFrom(t).withTpe(Definitions.strType).withCV(CaptureSet.empty))
      case Syntax.Term.IntLit(value) => 
        val tpe = if expected.exists && expected.isIntegralType then expected else Definitions.i32Type
        Right(Term.IntLit(value).withPosFrom(t).withTpe(tpe).withCV(CaptureSet.empty))
      case Syntax.Term.BoolLit(value) =>
        val tpe = Definitions.boolType
        Right(Term.BoolLit(value).withPosFrom(t).withTpe(tpe).withCV(CaptureSet.empty))
      case Syntax.Term.UnitLit() => 
        Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType).withCV(CaptureSet.empty))
      case Syntax.Term.Select(base, field) =>
        checkSelect(base, field, t.pos)
      case Syntax.Term.Assign(lhs, rhs) =>
        checkAssign(lhs, rhs, t.pos)
      case Syntax.Term.Infix(op, lhs, rhs) =>
        checkInfix(op, lhs, rhs, expected, t.pos)
      case Syntax.Term.Prefix(op, term) =>
        checkPrefix(op, term, expected, t.pos)
      case Syntax.Term.If(cond, thenBranch, maybeElseBranch) =>
        checkIf(cond, thenBranch, maybeElseBranch, expected, t.pos)
      case Syntax.Term.Lambda(params, body) => 
        hopefully:
          val ps1 = checkTermParamList(params).!!
          def checkBody(using Context): Result[Term] =
            checkTerm(body)
          checkTermAbstraction(ps1, checkBody, t.pos).!!
      case Syntax.Term.TypeLambda(params, body) =>
        hopefully:
          val params1 = checkTypeParamList(params).!!
          def checkBody(using Context): Result[Term] =
            checkTerm(body)
          checkTypeAbstraction(params1, checkBody, t.pos).!!
      case Syntax.Term.Block(stmts) => 
        def avoidSelfType(tpe: Type, d: Syntax.Definition): Result[Type] =
          val approxType = Type.Capturing(tpe.stripCaptures, CaptureSet.universal)
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
                case d: Syntax.Definition.ExtensionDef =>
                  sorry(TypeError.GeneralError("extension definitions are not allowed in a block").withPos(d.pos))
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
                val bodyExpr = go(ds)(using ctx.extend(bd1 :: Nil).moreConsumedPeaks(peaksConsumed)).!!

                val resType = bodyExpr.tpe
                val approxElems = bd1.tpe.captureSet.elems.flatMap: cref =>
                  // if bd1.localCapInsts.contains(cref) then 
                  //   // drop local cap instances, since they mean "fresh" things
                  //   None
                  // else Some(cref)
                  Some(cref)
                val approxCs = CaptureSet(approxElems)
                val approxType = Type.Capturing(boundExpr1.tpe.stripCaptures, approxCs)
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
        checkPrimOp(PrimitiveOp.fromName(name).get, args, expected, t.pos).map(instantiateFresh)
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if PrimitiveOp.fromName(name).isDefined =>
        val primOp = PrimitiveOp.fromName(name).get
        checkPolyPrimOp(primOp, targs, args, expected, t.pos).map(instantiateFresh)
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if lookupStructSymbol(name).isDefined =>
        val classSym = lookupStructSymbol(name).get
        checkStructInit(classSym, targs, args, expected, t.pos).map(instantiateFresh)
      case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if lookupStructSymbol(name).isDefined =>
        val classSym = lookupStructSymbol(name).get
        checkStructInit(classSym, targs = Nil, args, expected, t.pos).map(instantiateFresh)
      case Syntax.Term.Apply(fun, args) => 
        checkApply(fun, args, expected, t.pos).map(instantiateFresh)
      case Syntax.Term.TypeApply(term, targs) => 
        hopefully:
          val term1 = checkTerm(term).!!
          term1.tpe.stripCaptures match
            case funTpe @ Type.TypeArrow(formals, resultType) => 
              val typeArgs = checkTypeArgs(targs, formals, t.pos).!!
              val captureArgs = typeArgs.collect:
                case cs: CaptureSet => cs
              val resultType1 = substituteType(resultType, typeArgs)
            //println(s"checkTypeApply $resultType --> $resultType1, typeArgs = $typeArgs")
              val resultTerm = Term.TypeApply(term1, typeArgs).withPosFrom(t).withTpe(resultType1)
              resultTerm.withCVFrom(term1)
              term1 match
                case _: VarRef =>
                case _ =>
                  resultTerm.withMoreCV(funTpe.captureSet)
              val signature = funTpe.signatureCaptureSet
              val todoCaptureSets = signature :: captureArgs
              for i <- 0 until todoCaptureSets.length do
                for j <- i + 1 until todoCaptureSets.length do
                  if !checkSeparation(todoCaptureSets(i), todoCaptureSets(j)) then
                    sorry(TypeError.SeparationError(todoCaptureSets(i).show, todoCaptureSets(j).show).withPos(t.pos))
              instantiateFresh(resultTerm)
            case _ => 
              sorry(TypeError.GeneralError(s"Expected a function, but got $term1.tpe.show").withPos(t.pos))

    result.flatMap: t1 =>
      if !expected.exists || TypeComparer.checkSubtype(t1.tpe, expected) then
        Right(t1)
      else 
        Left(TypeError.TypeMismatch(expected.show, t1.tpe.show).withPos(t.pos))

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
      val elseBranch1 = checkTerm(elseBranch, expected = thenBranch1.tpe).!!
      val finalTpe = thenBranch1.tpe
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
        Left(TypeError.GeneralError(s"Unsupported infix operation: $op").withPos(srcPos))

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

  def consumedPeaks(cv: CaptureSet)(using Context): CaptureSet =
    val pks = computePeak(cv)
    val elems1 = pks.elems.filter: cref =>
      cref match
        case QualifiedRef(AccessMode.Consume(), core) => true
        case _ => false
    CaptureSet(elems1)

  /** Check a function apply.
   * Returns the arguments, the result type, and the consumed capabilities in this apply.
   */
  def checkFunctionApply(funType: Type, args: List[Syntax.Term], srcPos: SourcePos, isDependent: Boolean = true)(using Context): Result[(List[Term], Type, CaptureSet)] =
    hopefully:
      funType.stripCaptures match
        case Type.TermArrow(formals, resultType) =>
          if args.length != formals.length then
            sorry(TypeError.GeneralError(s"Argument number mismatch, expected ${formals.length}, but got ${args.length}").withPos(srcPos))
          def go(xs: List[(Syntax.Term, TermBinder)], acc: List[Term], captureSetAcc: List[(Boolean, CaptureSet)]): (List[Term], Type, List[(Boolean, CaptureSet)]) = xs match
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
                (formal.isConsume, cs)
              go(xs, arg1 :: acc, css ++ captureSetAcc)
          val (args1, outType, css) = go(args `zip` (formals), Nil, Nil)
          // perform separation check
          val sigCaptures = funType.signatureCaptureSet
          val css1 = (css.map(_._2)) ++ (sigCaptures :: Nil)
          for i <- 0 until css1.length do
            for j <- i + 1 until css1.length do
              if !checkSeparation(css1(i), css1(j)) then
                sorry(TypeError.SeparationError(css1(i).show, css1(j).show).withPos(srcPos))
          val toConsume = css.filter(_._1).map(_._2).reduceLeftOption(_ ++ _).getOrElse(CaptureSet.empty)
          val consumedSet = tryConsume(toConsume, srcPos).!!
          (args1, outType, consumedSet)
        case _ => sorry(TypeError.GeneralError(s"Expected a function, but got ${funType.show}").withPos(srcPos))

  /** Instantiate fresh capabilities in the result of a function apply. */
  def instantiateFresh(t: Term)(using Context): Term =
    val tpe = t.tpe
    val tpe1 = instantiateCaps(tpe, isFresh = true)
    val t1 = t.withTpe(tpe1)
    t1

  def checkApply(fun: Syntax.Term, args: List[Syntax.Term], expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val fun1 = checkTerm(fun).!!
      val funType = fun1.tpe
      funType.stripCaptures match
        case Type.TermArrow(formals, resultType) =>
          val (args1, outType, consumedSet) = checkFunctionApply(funType, args, srcPos).!!
          //println(s"consumed set = ${consumedSet.show}")
          val resultTerm = Term.Apply(fun1, args1).withPos(srcPos).withTpe(outType)
          resultTerm.withCVFrom(fun1 :: args1*).withMoreCV(consumedSet)
          fun1 match
            case _: VarRef =>
              // skip, since it will already be marked
            case _ =>
              resultTerm.withMoreCV(fun1.tpe.captureSet)
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
        case _ =>
          sorry(TypeError.GeneralError(s"Expected a function, but got ${funType.show}").withPos(fun.pos))

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
      case _ => Left(TypeError.GeneralError(s"Argument number mismatch for primitive operation, expected ${formals.length}, but got ${args.length}").withPos(pos))
    go(args, formals, Nil).map: args1 =>
      Term.PrimOp(op, Nil, args1).withPos(pos).withTpe(Type.Base(resType).withKind(TypeKind.Star)).withCVFrom(args1*)

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
      case PrimitiveOp.Sorry =>
        hopefully:
          if expected.exists then
            Term.PrimOp(PrimitiveOp.Sorry, Nil, Nil).withPos(pos).withTpe(expected).withCV(CaptureSet.empty)
          else sorry(TypeError.GeneralError("no expected type for sorry").withPos(pos))
      case PrimitiveOp.ArrayNew =>
        hopefully:
          given ctx1: Context = ctx.newInferenceScope
          val tv = Inference.createTypeVar()
          val resultTerm = checkArrayInit(tv, args, pos).!!
          Inference.solveTypeVars()
          resultTerm
      case _ => assert(false, s"Unsupported primitive operation: $op")

  def checkArrayInit(elemType: Type, args: List[Syntax.Term], pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val arrConsFunction = Type.TermArrow(
        List(
          TermBinder("size", Definitions.i32Type, isConsume = false),
          TermBinder("elemType", elemType, isConsume = false), 
        ),
        Type.Capturing(Type.AppliedType(Type.Base(BaseType.ArrayType), List(elemType)), CaptureSet.universal)
      )
      val (args1, outType, consumedSet) = checkFunctionApply(arrConsFunction, args, pos, isDependent = false).!!
      Term.PrimOp(PrimitiveOp.ArrayNew, elemType :: Nil, args1).withPos(pos).withTpe(outType).withCVFrom(args1*)
      
  def checkPolyPrimOp(op: PrimitiveOp, targs: List[Syntax.Type | Syntax.CaptureSet], args: List[Syntax.Term], expected: Type, pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      op match
        case PrimitiveOp.ArrayNew =>
          (targs, args) match
            case ((elemType : Syntax.Type) :: Nil, arg1 :: arg2 :: Nil) =>
              val elemType1 = checkType(elemType).!!
              checkArrayInit(elemType1, args, pos).!!
            case _ => sorry(TypeError.GeneralError(s"Argument number mismatch, `newArray` expects one type argument and two term arguments"))
        case _ => sorry(TypeError.GeneralError(s"Primitive operation $op cannot be applied to type arguments").withPos(pos))

  def checkStructDef(d: Syntax.Definition.StructDef)(using Context): Result[StructInfo] =
    hopefully:
      val binders = checkTypeParamList(d.targs).!!
      val ctx1 = ctx.extend(binders)
      val fieldNames = d.fields.map(_.name)
      if fieldNames.distinct.length != fieldNames.length then
        sorry(TypeError.GeneralError("Duplicated field name").withPos(d.pos))
      val fields = d.fields.map: fieldDef =>
        val fieldType = checkType(fieldDef.tpe)(using ctx1).!!
        FieldInfo(fieldDef.name, fieldType, fieldDef.isVar)
      StructInfo(binders, fields)

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
        def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[Term] = pss match
          case Nil => 
            val expectedBodyType = resultType match
              case None => Type.NoType()
              case Some(expected) => checkType(expected).!!
            val tm = UniversalConversion()
            val expected1 = tm.apply(expectedBodyType)
            val outTerm = checkTerm(expr, expected = expected1)
            val localSets = tm.createdUniversals
            val css = localSets.map(_.solve())
            // Perform separation checking
            for i <- 0 until css.size do
              for j <- i + 1 until css.size do
                val ci = css(i)
                val cj = css(j)
                if !checkSeparation(ci, cj) then
                  sorry(TypeError.GeneralError(s"${ci.show} and ${cj.show} consumes the same capability twice"))
            // Check the level of fresh caps
            val curLevel = ctx.freshLevel
            css.foreach: cs =>
              val pks = computePeak(cs)
              pks.elems.foreach: cref =>
                cref.core match
                  case CaptureRef.CapInst(capId, CapKind.Fresh(level), fromInst) =>
                    if level < curLevel then
                      sorry(TypeError.GeneralError(s"Treating capabilities ${cs.show} as fresh in the result type but they come from the outside").withPos(expr.pos))
                  case CaptureRef.CapInst(capId, CapKind.Sep(level), fromInst) =>
                    sorry(TypeError.GeneralError(s"Treating capabilities ${cs.show} as fresh in the result type but they are not fresh").withPos(expr.pos))
                  case _ =>
            outTerm.foreach: t =>
              if expectedBodyType.exists then
                t.setTpe(expectedBodyType)
            outTerm
          case Syntax.TermParamList(params) :: pss => 
            val params1 = checkTermParamList(params).!!
            def checkBody(using Context): Result[Term] = go(pss)
            checkTermAbstraction(params1, checkBody, srcPos = d.pos)
          case Syntax.TypeParamList(params) :: pss =>
            val params1 = checkTypeParamList(params).!!
            def checkBody(using Context): Result[Term] = go(pss)
            checkTypeAbstraction(params1, checkBody, srcPos = d.pos)
        val pss1: List[Syntax.TermParamList | Syntax.TypeParamList] = paramss match
          case Nil => List(Syntax.TypeParamList(Nil))
          case pss => pss
        val expr1 = go(pss1).!!
        val bd = TermBinder(name, expr1.tpe, isConsume = false).withPos(d.pos)
        (bd.asInstanceOf[TermBinder], expr1)

  def extractDefType(d: Syntax.Definition.DefDef, requireExplictType: Boolean = true)(using Context): Result[Type] = 
    def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[Type] = pss match
      case Nil => 
        d.resultType match
          case None => 
            if requireExplictType then Left(TypeError.GeneralError("Explicit type annotation is required for top-level definitions").withPos(d.pos))
            else Right(Definitions.anyType)
          case Some(resultType) => checkType(resultType)
      case (ps: Syntax.TermParamList) :: pss =>
        checkTermParamList(ps.params).flatMap: params =>
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
      case None => Left(TypeError.GeneralError("Explicit type annotation is required for top-level definitions").withPos(d.pos))
      case Some(expected) => checkType(expected)

  def extractDefnType(d: Syntax.ValueDef)(using Context): Result[Type] = d match
    case d: Syntax.Definition.ValDef => extractValType(d)
    case d: Syntax.Definition.DefDef => extractDefType(d)

  def checkModule(defns: List[Syntax.Definition])(using Context): Result[Module] = 
    val mod = Expr.Module(Nil)
    def hasDuplicatedName: Boolean =
      val names = defns.map(_.name)
      names.distinct.length != names.length
    if hasDuplicatedName then
      Left(TypeError.GeneralError("Duplicated definition name").withPos(defns.head.pos))
    else
      hopefully:
        // Create symbols for all definitions
        val syms = defns.map: defn =>
          defn match
            case defn: (Syntax.Definition.ValDef | Syntax.Definition.DefDef) => 
              //val tpe = extractDefnType(defn).!!
              DefSymbol(defn.name, Definitions.anyType, mod).withPosFrom(defn)
            case sd: Syntax.Definition.StructDef =>
              val tparams = checkTypeParamList(sd.targs).!!
              StructSymbol(defn.name, StructInfo(tparams, Nil), mod).withPosFrom(defn)
            case ed: Syntax.Definition.ExtensionDef =>
              val tparams = checkTypeParamList(ed.typeArgs).!!
              val info = ExtensionInfo(tparams, Definitions.anyType, Nil)
              ExtensionSymbol(defn.name, info, mod).withPosFrom(ed)
        def checkDefns(defns: List[(DefSymbol, Syntax.ValueDef)]): Result[List[Expr.Definition]] = defns match
          case Nil => Right(Nil)
          case (sym, defn) :: defns =>
            val ctx1 = 
              if defn.isInstanceOf[Syntax.Definition.ValDef] then
                // val defs may only depend on previous definitions
                ctx.addSymbols(syms.takeWhile(_.name != defn.name))
              else
                ctx.addSymbols(syms)
            checkDef(defn)(using ctx1).flatMap: (bd, expr) =>
              checkDefns(defns).map: defns1 =>
                val d = Definition.ValDef(sym, expr)
                d :: defns1
        def checkExtensionDef(extSym: ExtensionSymbol, extDefn: Syntax.Definition.ExtensionDef)(using Context): Result[Definition.ExtensionDef] =
          hopefully:
            val ctx1 = ctx.addSymbols(syms).extend(extSym.info.typeParams)
            val methodInfos = extDefn.methods.map: method =>
              val method1 =
                method.copy(paramss = Syntax.TermParamList(List(extDefn.selfArg)) :: method.paramss).withPosFrom(method)
              val (bd, expr) = checkDef(method1)(using ctx1).!!
              ExtensionMethod(method.name, bd.tpe, expr)
            val newInfo = extSym.info.copy(methods = methodInfos)
            extSym.info = newInfo
            Definition.ExtensionDef(extSym)
        // Typecheck struct definitions
        val structDefTodos: List[(StructSymbol, Syntax.Definition.StructDef)] = (syms `zip` defns).flatMap:
          case (sym: StructSymbol, defn: Syntax.Definition.StructDef) => Some((sym, defn))
          case _ => None
        val structDefns: List[Definition.StructDef] = structDefTodos.map: (sym, defn) =>
          val ctx1 = ctx.addSymbols(syms)
          val info = checkStructDef(defn)(using ctx1).!!
          sym.info = info
          Definition.StructDef(sym)
        val allClassSyms = structDefns.map(_.sym)
        val ctxWithClasses = ctx.addSymbols(allClassSyms)
        // Assign declared types to value definitions and extension definitions
        for ((sym, defn) <- syms `zip` defns) do
          (sym, defn) match
            case (sym: DefSymbol, defn: Syntax.Definition.ValDef) =>
              val defnType = extractDefnType(defn)(using ctxWithClasses).!!
              sym.tpe = defnType
            case (sym: DefSymbol, defn: Syntax.Definition.DefDef) =>
              val defnType = extractDefnType(defn)(using ctxWithClasses).!!
              sym.tpe = defnType
            case (sym: ExtensionSymbol, extDefn: Syntax.Definition.ExtensionDef) =>
              val typeArgs = sym.info.typeParams
              val ctx1 = ctxWithClasses.extend(typeArgs)
              val selfArgBinder = checkTermParam(extDefn.selfArg)(using ctx1).!!
              val extMethods = extDefn.methods.map: defn =>
                val defn1 = defn.copy(paramss = Syntax.TermParamList(List(extDefn.selfArg)) :: defn.paramss).withPosFrom(defn)
                val methodType = extractDefnType(defn1)(using ctx1).!!
                ExtensionMethod(defn1.name, methodType, body = null)
              sym.info = ExtensionInfo(typeArgs, selfArgBinder.tpe, extMethods)
            case _ =>
        // Typecheck extension definitions
        val extensionDefTodos: List[(ExtensionSymbol, Syntax.Definition.ExtensionDef)] = (syms `zip` defns).flatMap:
          case (sym: ExtensionSymbol, defn: Syntax.Definition.ExtensionDef) => Some((sym, defn))
          case _ => None
        val extensionDefns = extensionDefTodos.map: (sym, defn) =>
          checkExtensionDef(sym, defn)(using ctxWithClasses).!!
        // Typecheck value definitions
        val valueDefTodos: List[(DefSymbol, Syntax.ValueDef)] = (syms `zip` defns).flatMap:
          case (sym: DefSymbol, defn: Syntax.ValueDef) => Some((sym, defn))
          case _ => None
        val valueDefns = checkDefns(valueDefTodos).!!
        // Done
        mod.defns = structDefns ++ extensionDefns ++ valueDefns
        mod

  def isStablePath(t: Term): Boolean = t match
    case Term.Select(base, fieldInfo) => isStablePath(base) && !fieldInfo.mutable
    case Term.SymbolRef(_) => true
    case Term.BinderRef(_) => true
    case _ => false

  /** Search for extention method who has a field of `field` and is applicable to `baseType` */
  def searchExtension(baseType: Type, field: String)(using Context): Option[(ExtensionSymbol, List[(Type | CaptureSet)])] = boundary:
    ctx.symbols.foreach:
      case sym: ExtensionSymbol if sym.info.methods.exists(_.name == field) =>
        given ctx1: Context = ctx.newInferenceScope
        val typeArgs: List[Type | CaptureSet] = sym.info.typeParams.map:
          case TypeBinder(name, bound) => createTypeVar(upperBound = bound)
          case CaptureBinder(name, bound) => 
            CaptureSet.empty // for now, capture set inference is not supported for extension search
        val selfArgType = substituteType(sym.info.selfArgType, typeArgs, isParamType = true)
        val tm = UniversalConversion()
        val formal = tm.apply(selfArgType)
        if TypeComparer.checkSubtype(baseType, formal) then
          solveTypeVars()
          break(Some((sym, typeArgs)))
        // SCOPE OF EXTENSION METHDOS ???
      case _ =>
    None

  def checkSelect(base: Syntax.Term, field: String, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val base1 = checkTerm(base).!!
      def fail: Result[Nothing] = Left(TypeError.TypeMismatch(s"a type with the field $field", base1.tpe.show).withPosFrom(base))
      def tryPrimArray: Result[Term] =
        hopefully:
          base1.tpe.stripCaptures match
            case PrimArrayType(elemType) =>
              field match
                case "size" | "length" =>
                  Term.PrimOp(PrimitiveOp.ArrayLen, Nil, List(base1)).withPos(srcPos).withTpe(Definitions.i32Type).withCV(base1.cv)
                case _ => fail.!!
            case _ => fail.!!
      def tryStruct: Result[Term] =
        hopefully:
          getFieldInfo(base1.tpe, field) match
            case Some(fieldInfo) =>
              val outTerm = Term.Select(base1, fieldInfo).withPos(srcPos).withTpe(fieldInfo.tpe).withCV(base1.cv)
              if isStablePath(outTerm) then
                val singletonSet = (outTerm.asInstanceOf[VarRef]).asSingletonType.singletonCaptureSet
                val narrowedType = Type.Capturing(outTerm.tpe.stripCaptures, singletonSet)
                outTerm.withTpe(narrowedType).withCV(singletonSet)
              else outTerm
            case _ => fail.!!
      def tryExtension: Result[Term] =
        hopefully:
          searchExtension(base1.tpe, field) match
            case Some((extSym, typeArgs)) =>
              val method = extSym.info.methods.find(_.name == field).get
              val funType = substituteType(method.tpe, typeArgs, isParamType = false)
              val (args, outType, consumedSet) = checkFunctionApply(funType, List(base), srcPos, isDependent = true).!!
              val resolvedTerm = Term.ResolveExtension(extSym, typeArgs, method.name).withPos(srcPos).withTpe(funType).withCV(CaptureSet.empty)
              val appliedTerm = Term.Apply(resolvedTerm, args).withPos(srcPos).withTpe(outType).withCVFrom(resolvedTerm :: args*)
              appliedTerm
            case None => fail.!!
      (tryPrimArray || tryStruct || tryExtension).!!

  def checkAssign(lhs: Syntax.Term, rhs: Syntax.Term, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      def fail = sorry(TypeError.GeneralError(s"Cannot assign to this target").withPosFrom(lhs))
      val lhs1 = checkTerm(lhs).!!
      lhs1 match
        case lhs1 @ Term.Select(_, fieldInfo) =>
          if fieldInfo.mutable then
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
      tpe match
        case AppliedStructType(classSym, targs) => 
          classSym.info.fields.find(_.name == field).map: fieldInfo =>
            val fieldType = substituteType(fieldInfo.tpe, targs, isParamType = false)
            FieldInfo(fieldInfo.name, fieldType, fieldInfo.mutable)
        case Type.Capturing(tpe, _) => go(tpe)
        case Type.RefinedType(core, refinements) =>
          refinements.find(_.name == field).orElse(go(core))
        case _ => None
    go(tpe)

  def allFieldNames(tp: Type)(using Context): List[String] = tp match
    // case Type.BinderRef(idx) => allFieldNames(getBinder(idx).asInstanceOf[TypeBinder].bound) // TODO: fix this case
    case Type.Capturing(inner, captureSet) => allFieldNames(inner)
    case AppliedStructType(classSym, targs) => 
      val fieldNames = classSym.info.fields.map(_.name)
      fieldNames
    case Type.RefinedType(base, refinements) =>
      allFieldNames(base) ++ refinements.map(_.name)
    case Type.Var(ref) => allFieldNames(ref.tpe)
    case Type.Select(base, fieldInfo) => allFieldNames(fieldInfo.tpe)
    case _ => Nil
  
