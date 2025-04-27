package cavia
package typechecking

import core.*
import ast.*
import scala.collection.mutable.ArrayBuffer
import cavia.core.ast.Syntax.PrefixOp
import reporting.trace

object TypeChecker:
  import Expr.*
  import Binder.*

  /** Type checking context. */
  case class Context(binders: List[Binder], symbols: List[Symbol]):
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

  object Context:
    def empty: Context = Context(Nil, Nil)

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
      val binder: TermBinder = TermBinder(param.name, tpe)
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
      case None => Right(Definitions.capCaptureSet.withPosFrom(param))
      case Some(captureSet) => checkCaptureSet(captureSet)
    bound.map: captureSet =>
      val binder: CaptureBinder = CaptureBinder(param.name, captureSet)
      binder.withPosFrom(param)

  def checkCaptureOrTypeParam(binder: (Syntax.CaptureParam | Syntax.TypeParam))(using ctx: Context): Result[CaptureBinder | TypeBinder] =
    binder match
      case binder: Syntax.CaptureParam => checkCaptureParam(binder)
      case binder: Syntax.TypeParam => checkTypeParam(binder)

  def checkCaptureRef(ref: Syntax.CaptureRef)(using ctx: Context): Result[CaptureRef] =
    if ref.name == "cap" then
      Right(CaptureRef.CAP().maybeWithPosFrom(ref))
    else lookupAll(ref.name) match
      case Some(sym: DefSymbol) => Right(CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))).maybeWithPosFrom(ref))
      case Some((binder: (Binder.CaptureBinder | Binder.TermBinder), idx)) => Right(CaptureRef.Ref(Type.Var(Term.BinderRef(idx))).maybeWithPosFrom(ref))
      case Some((binder: Binder.TypeBinder, idx)) => Left(TypeError.UnboundVariable(ref.name, s"I found a type name, but was looking for either a term or capture name").withPos(ref.pos))
      case _ => Left(TypeError.UnboundVariable(ref.name).withPos(ref.pos))

  def checkCaptureSet(captureSet: Syntax.CaptureSet)(using ctx: Context): Result[CaptureSet] =
    val checkElems: List[Result[CaptureRef]] = captureSet.elems.map(checkCaptureRef)
    @annotation.tailrec
    def go(elems: List[Result[CaptureRef]], acc: List[CaptureRef]): Result[CaptureSet] = elems match
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
    def goRef(ref: CaptureRef): Set[CaptureRef] = 
      //println(s"goRef $ref")
      ref match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) =>
          getBinder(idx) match
            case Binder.TermBinder(_, tpe, _) =>
              val elems = tpe.captureSet.elems
              goRefs(elems)
            case _: Binder.CaptureBinder => Set(ref)
            case _ => assert(false, "malformed capture set")
        case CaptureRef.Ref(Type.Var(Term.SymbolRef(sym))) =>
          val refs = sym.tpe.captureSet.elems
          goRefs(refs)
        case CaptureRef.Ref(Type.Select(base, fieldInfo)) => 
          assert(false, "TODO: implement")
        case CaptureRef.CAP() => Set(ref)
        case CaptureRef.CapInst(capId) => Set(ref)
    def goRefs(refs: List[CaptureRef]): Set[CaptureRef] =
      //println(s"goRefs $refs")
      refs.flatMap(goRef).toSet
    val elems = goRefs(set.elems)
    CaptureSet(elems.toList)

  def checkSeparation(cs1: CaptureSet, cs2: CaptureSet)(using Context): Boolean = //trace(s"checkSeparation ${cs1.show} ${cs2.show}"):
    val pk1 = computePeak(cs1)
    val pk2 = computePeak(cs2)
    val intersection = pk1.elems.intersect(pk2.elems)
    intersection.isEmpty

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
    case Syntax.Type.Capturing(inner, captureSet) =>
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
            val targs1 = args.map(arg => checkType(arg).!!)
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

  // def markFree(cs: CaptureSet, srcPos: SourcePos)(using Context): Result[Unit] =
  //   hopefully:
  //   //println(s"markFree $cs")
  //     val crefs = cs.elems.flatMap: ref =>
  //       ref match
  //         case CaptureRef.Ref(ref) => Some(ref)
  //         case CaptureRef.CapInst(capId) => 
  //           sorry(TypeError.GeneralError(s"A `{cap}` that is ambiguous is charged to the environment; consider using explicit capture parameters").withPos(srcPos))
  //         case CaptureRef.CAP() =>
  //           sorry(TypeError.GeneralError(s"A CAP() template is charged to the environment; consider using explicit capture parameters").withPos(srcPos))
  //     crefs.foreach(markFree)

  // def markFree(ref: VarRef)(using Context): Unit =
  //   //println(s"markFree $ref")
  //   def widenUntilWf(crefs: List[CaptureRef], delta: Int): List[CaptureRef] =
  //     def allWf(crefs: List[CaptureRef]): Boolean = crefs.forall: ref =>
  //       ref match
  //         case CaptureRef.Ref(Term.BinderRef(idx)) => idx >= delta
  //         case _ => true
  //     if allWf(crefs) then
  //       crefs.map: ref =>
  //         ref match
  //           case CaptureRef.Ref(Term.BinderRef(idx)) =>
  //             //assert(idx >= delta)
  //             CaptureRef.Ref(Term.BinderRef(idx - delta))
  //           case _ => ref
  //     else
  //       val crefs1 = crefs.flatMap: ref =>
  //         ref match
  //           case CaptureRef.Ref(Term.BinderRef(idx)) if idx < delta =>
  //             getBinder(idx) match
  //               case TermBinder(name, tpe, _) => tpe.captureSet.elems
  //               case CaptureBinder(name, bound) => bound.elems
  //               case _ => assert(false)
  //           case ref => List(ref)
  //       widenUntilWf(crefs1, delta)

  //   def avoidVarRef(ref: VarRef, delta: Int): List[CaptureRef] = ref match
  //     case Term.BinderRef(idx) => 
  //       if idx >= delta then
  //         List(Term.BinderRef(idx - delta).asCaptureRef)
  //       else
  //         //println(s"!!! markFree: dropping local binder $ref")
  //         widenUntilWf(List(ref.asCaptureRef), delta)
  //     case Term.SymbolRef(sym) => List(ref.asCaptureRef)
  //   val level = ctx.captured.level
  //   val curLevel = ctx.binders.length
  //   val delta = curLevel - level
  //   //println(s"level = $level, curLevel = $curLevel, delta = $delta")
  //   assert(delta >= 0, s"absurd levels")
  //   val crefs = avoidVarRef(ref, delta)
  //   //println(s"markFree $ref ==> $crefs")
  //   ctx.captured.add(crefs.toSet)

  private def dropLocalParams(crefs: List[CaptureRef], numParams: Int): (Boolean, List[CaptureRef]) = 
    var existsLocalParams = false
    val newCrefs = crefs.flatMap: ref =>
      ref match
        case CaptureRef.Ref(Type.Var(Term.BinderRef(idx))) =>
          if idx >= numParams then
            Some(CaptureRef.Ref(Type.Var(Term.BinderRef(idx - numParams))).maybeWithPosFrom(ref))
          else 
            existsLocalParams = true
            None
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
                if f.elems.contains(CaptureRef.CAP()) || TypeComparer.checkSubcapture(cs1, f1) then
                  cs1
                else sorry(TypeError.GeneralError(s"Capture set argument ${cs1.show} does not conform to the bound ${f.show}").withPosFrom(t))
              case (t, f) => sorry(TypeError.GeneralError("Argument kind mismatch").withPosFrom(t))
          // val fs1 = fs.zipWithIndex.map: (f, i) =>
          //   substituteType(f, typeArg, i, isParamType = true)
          go(ts, fs, typeArg :: checkedAcc)
        case _ => 
          sorry(TypeError.GeneralError(s"Type argument number mismatch: expected ${formals.length}, but got ${targs.length}").withPos(srcPos))
      go(targs, formalTypes, Nil)

  def instantiateBinderCaps(binder: TermBinder)(using Context): TermBinder =
    val tm = CapInstantiation()
    val tpe1 = tm.apply(binder.tpe)
    val binder1 = TermBinder(binder.name, tpe1, tm.localCaps)
    binder1.maybeWithPosFrom(binder).asInstanceOf[TermBinder]

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
      val syntheticFunctionType = Type.TermArrow(argFormals.map(tpe => TermBinder("_", tpe)), Type.AppliedType(Type.SymbolRef(classSym), typeArgs))
      val (termArgs, _) = checkFunctionApply(syntheticFunctionType, args, expected, srcPos, isDependent = false).!!
      val classType = 
        if typeArgs.isEmpty then
          Type.SymbolRef(classSym)
        else
          Type.AppliedType(Type.SymbolRef(classSym), typeArgs)
      val termCaptureElems = termArgs.flatMap: arg =>
        arg.tpe.captureSet.elems
      val refinements: List[FieldInfo] = (fields1 `zip` termArgs).flatMap: 
        case ((name, fieldType, mutable), arg) =>
          if fieldType.isPure then None
          else Some(FieldInfo(name, arg.tpe, mutable))
      val captureSet = CaptureSet(CaptureRef.CAP() :: termCaptureElems)
      val outType = Type.Capturing(classType, captureSet)
      val refinedOutType =
        if !refinements.isEmpty then
          Type.RefinedType(outType, refinements)
        else outType
      Term.StructInit(classSym, typeArgs, termArgs).withPos(srcPos).withTpe(refinedOutType).withCVFrom(termArgs*)

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
          tpe.stripCaptures match
            case LazyType(resultType) => 
              val t1 = Term.TypeApply(ref, Nil).withPosFrom(t)
              t1.withTpe(resultType).withCV(cv)
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
        checkTermParamList(params).flatMap: params =>
          val params1 = params.map(instantiateBinderCaps)
          val ctx1 = ctx.extend(params1)
          checkTerm(body)(using ctx1).map: body1 =>
            val t1 = Term.TermLambda(params, body1).withPosFrom(t)
            val cv = body1.cv
            val (_, cv1) = dropLocalParams(cv.elems.toList, params.length)
            val tpe = Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star)
            val tpe1 = Type.Capturing(tpe.stripCaptures, CaptureSet(cv1)).withKind(TypeKind.Star)
            val lambdaCV = CaptureSet.empty  // values in Capybara have an empty cv
            t1.withTpe(tpe1).withCV(lambdaCV)
      case Syntax.Term.TypeLambda(params, body) =>
        hopefully:
          val params1 = checkTypeParamList(params).!!
          val ctx1 = ctx.extend(params1)
          val body1 = checkTerm(body)(using ctx1).!!
          val t1 = Term.TypeLambda(params1, body1).withPosFrom(t)
          val cv = body1.cv
          val (existsLocalParams, cv1) = dropLocalParams(cv.elems.toList, params.length)
          if existsLocalParams then
            sorry(TypeError.GeneralError("local capture parameters captured by the body of a the lambda"))
          val tpe = Type.TypeArrow(params1, body1.tpe).withKind(TypeKind.Star)
          val tpe1 = Type.Capturing(tpe.stripCaptures, CaptureSet(cv1)).withKind(TypeKind.Star)
          val lambdaCV = CaptureSet.empty  // values in Capybara have an empty cv
          t1.withTpe(tpe1).withCV(lambdaCV)
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
              val ctx1 = selfType match
                case None => ctx
                case Some(selfType) =>
                  val selfBinder = TermBinder(d.name, selfType).withPos(d.pos)
                  ctx.extend(selfBinder)
              val (bd, expr) = checkDef(d.asInstanceOf[Syntax.ValueDef])(using ctx1).!!
              // Avoid self reference
              val bd1 = 
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(bd.tpe, d).!!
                  TermBinder(bd.name, tpe1).withPos(bd.pos).asInstanceOf[TermBinder]
                else bd
              // Instantiate CAPs in the type
              val bd2 = instantiateBinderCaps(bd1)
              // Avoid self type in the body
              val expr1 =
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(expr.tpe, d).!!
                  expr.withTpe(tpe1)
                else expr
              (bd2, expr1, selfType.isDefined)
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
                val bodyExpr = go(ds)(using ctx.extend(bd1 :: Nil)).!!
                // drop local cap instances from the cv of the body
                //dropLocalCapInsts(bd1.localCapInsts)
                bodyExpr.withCV:
                  val oldCV = bodyExpr.cv
                  val newCV = oldCV.elems.filter: cref =>
                    !bd1.localCapInsts.contains(cref)
                  CaptureSet(newCV)

                val resType = bodyExpr.tpe
                val approxElems = bd1.tpe.captureSet.elems.flatMap: cref =>
                  if bd1.localCapInsts.contains(cref) then 
                    // drop local cap instances, since they mean "fresh" things
                    None
                  else Some(cref)
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
        checkPrimOp(PrimitiveOp.fromName(name).get, args, expected, t.pos)
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if PrimitiveOp.fromName(name).isDefined =>
        val primOp = PrimitiveOp.fromName(name).get
        checkPolyPrimOp(primOp, targs, args, expected, t.pos)
      case Syntax.Term.Apply(Syntax.Term.TypeApply(Syntax.Term.Ident(name), targs), args) if lookupStructSymbol(name).isDefined =>
        val classSym = lookupStructSymbol(name).get
        checkStructInit(classSym, targs, args, expected, t.pos)
      case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if lookupStructSymbol(name).isDefined =>
        val classSym = lookupStructSymbol(name).get
        checkStructInit(classSym, targs = Nil, args, expected, t.pos)
      case Syntax.Term.Apply(fun, args) => 
        checkApply(fun, args, expected, t.pos)
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
              // term1 match
              //   case _: VarRef =>
              //   case _ =>
              //     markFree(funTpe.captureSet, t.pos)
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
              resultTerm
            case _ => 
              sorry(TypeError.GeneralError(s"Expected a function, but got $term1.tpe.show").withPos(t.pos))

    result.flatMap: t1 =>
      if !expected.exists || TypeComparer.checkSubtype(t1.tpe, expected) then
        //val t2 = if expected.exists then t1.withTpe(expected) else t1
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
      case PrefixOp.Neg =>
        val primOp = expected match
          case Type.Base(BaseType.I32) => PrimitiveOp.I32Neg
          case Type.Base(BaseType.I64) => PrimitiveOp.I64Neg
          case _ => PrimitiveOp.I32Neg
        checkPrimOp(primOp, List(term), expected, srcPos)
      case PrefixOp.Not =>
        val primOp = PrimitiveOp.BoolNot
        checkPrimOp(primOp, List(term), expected, srcPos)

  def checkFunctionApply(funType: Type, args: List[Syntax.Term], expected: Type, srcPos: SourcePos, isDependent: Boolean = true)(using Context): Result[(List[Term], Type)] =
    hopefully:
      funType.stripCaptures match
        case Type.TermArrow(formals, resultType) =>
          if args.length != formals.length then
            sorry(TypeError.GeneralError(s"Argument number mismatch, expected ${formals.length}, but got ${args.length}").withPos(srcPos))
          def go(xs: List[(Syntax.Term, Type)], acc: List[Term], captureSetAcc: List[CaptureSet]): (List[Term], Type, List[CaptureSet]) = xs match
            case Nil =>
              val args = acc.reverse
              (args, substitute(resultType, args), captureSetAcc)
            case (arg, formal) :: xs => 
              val formal1 =
                if isDependent then
                  substitute(formal, acc.reverse, isParamType = true)
                else formal
              val tm = UniversalConversion()
              val formal2 = tm.apply(formal1)
              val localSets = tm.createdUniversals
              //println(s"checkArg $arg, expected = $formal1 (from $formal)")
              val arg1 = checkTerm(arg, expected = formal2).!!
              val css = localSets.map(_.solve())
              // val xs1 = 
              //   if isDependent then
              //     xs.zipWithIndex.map:
              //       case ((arg, formal), idx) =>
              //         (arg, substitute(formal, arg1, idx, isParamType = true).!!)
              // else xs
              go(xs, arg1 :: acc, css ++ captureSetAcc)
          val (args1, outType, css) = go(args `zip` (formals.map(_.tpe)), Nil, Nil)
          // perform separation check
          val sigCaptures = funType.signatureCaptureSet
          val css1 = sigCaptures :: css
          for i <- 0 until css1.length do
            for j <- i + 1 until css1.length do
              if !checkSeparation(css1(i), css1(j)) then
                sorry(TypeError.SeparationError(css1(i).show, css1(j).show).withPos(srcPos))
          (args1, outType)
        case _ => sorry(TypeError.GeneralError(s"Expected a function, but got ${funType.show}").withPos(srcPos))

  def checkApply(fun: Syntax.Term, args: List[Syntax.Term], expected: Type, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val fun1 = checkTerm(fun).!!
      val funType = fun1.tpe
      funType.stripCaptures match
        case Type.TermArrow(formals, resultType) =>
          val (args1, outType) = checkFunctionApply(funType, args, expected, srcPos).!!
          val resultTerm = Term.Apply(fun1, args1).withPos(srcPos).withTpe(outType)
          // fun1 match
          //   case _: VarRef =>
          //     // Skip, since it will already be marked
          //   case _ =>
          //     markFree(fun1.tpe.captureSet, fun.pos)
          resultTerm.withCVFrom(fun1 :: args1*)
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
              ).withPos(srcPos).withTpe(elemType).withCV(CaptureSet.empty)
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
    val tm = TypeSubstitutionMap(targs, if isParamType then Variance.Contravariant else Variance.Covariant)
    tm.apply(tpe)

  def substituteTypeInCaptureSet(cs: CaptureSet, targs: List[Type | CaptureSet], isParamType: Boolean = false)(using Context): CaptureSet =
    val tm = TypeSubstitutionMap(targs, if isParamType then Variance.Contravariant else Variance.Covariant)
    tm.mapCaptureSet(cs)

  // def substitute(tpe: Type, arg: Term, openingIdx: Int = 0, isParamType: Boolean = false): Result[Type] =
  //   val startingVariance = if isParamType then Variance.Contravariant else Variance.Covariant
  //   arg match
  //     case ref: VarRef =>
  //       val ref1: VarRef = ref match
  //         case Term.BinderRef(idx) => Term.BinderRef(idx + openingIdx).maybeWithPosFrom(ref).asInstanceOf[VarRef]
  //         case Term.SymbolRef(sym) => ref
  //         case Term.Select(base, field) => assert(false, "not supported yet")
  //       val tm = OpenTermBinderExact(ref1, openingIdx, startingVariance)
  //       Right(tm.apply(tpe))
  //     case _ => 
  //       val argType = arg.tpe.shift(openingIdx)
  //       val tm = OpenTermBinder(argType, openingIdx, startingVariance)
  //       val result = tm.apply(tpe)
  //       if tm.ok then
  //         Right(result)
  //       else
  //         Left(TypeError.GeneralError(s"Cannot substitute $arg into $tpe because the argument occurs in a invariant position").withPos(arg.pos))

  // def substituteType[X <: Type | CaptureSet](tpe: X, arg: (Type | CaptureSet), openingIdx: Int = 0, isParamType: Boolean = false): X = 
  //   val tm = arg match
  //     case tpe: Type => OpenTypeBinder(tpe.shift(openingIdx), openingIdx)
  //     case captureSet: CaptureSet => OpenCaptureBinder(captureSet.shift(openingIdx), openingIdx)
  //   tpe match
  //     case tpe: Type => tm.apply(tpe).asInstanceOf[X]
  //     case captureSet: CaptureSet => tm.mapCaptureSet(captureSet).asInstanceOf[X]

  // def substituteAll(tpe: Type, args: List[Term], isParamType: Boolean = false): Result[Type] =
  //   args match
  //     case Nil => Right(tpe)
  //     case arg :: args =>
  //       hopefully:
  //         val tpe1 = substitute(tpe, arg, openingIdx = args.length, isParamType).!!
  //         val tpe2 = substituteAll(tpe1, args, isParamType).!!
  //         tpe2

  // def substituteAllType(tpe: Type, args: List[(Type | CaptureSet)], isParamType: Boolean = false): Type =
  //   args match
  //     case Nil => tpe
  //     case arg :: args =>
  //       val tpe1 = substituteType(tpe, arg, openingIdx = args.length, isParamType)
  //       //println(s"open $arg in $tpe (idx=${args.length}) = $tpe1")
  //       substituteAllType(tpe1, args, isParamType)

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
      case _ => assert(false, s"Unsupported primitive operation: $op")
      
  def checkPolyPrimOp(op: PrimitiveOp, targs: List[Syntax.Type | Syntax.CaptureSet], args: List[Syntax.Term], expected: Type, pos: SourcePos)(using Context): Result[Term] =
    hopefully:
      op match
        case PrimitiveOp.ArrayNew =>
          (targs, args) match
            case ((elemType : Syntax.Type) :: Nil, arg1 :: arg2 :: Nil) =>
              val elemType1 = checkType(elemType).!!
              val arrayLength = checkTerm(arg1, expected = Definitions.i32Type).!!
              val arrayInit = checkTerm(arg2, expected = elemType1).!!
              val tpe = Type.Capturing(Definitions.arrayType(elemType1), CaptureSet.universal)
              Term.PrimOp(PrimitiveOp.ArrayNew, elemType1 :: Nil, arrayLength :: arrayInit :: Nil).withPos(pos).withTpe(tpe).withCVFrom(arrayLength, arrayInit)
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
        val bd = TermBinder(name, binderType).withPos(d.pos)
        (bd.asInstanceOf[TermBinder], expr1)
    case Syntax.Definition.DefDef(name, _, paramss, resultType, expr) => 
      def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[(Term, Option[CaptureSet])] = pss match
        case Nil => 
          //val env1 = CaptureEnv.empty(ctx.binders.length)
          hopefully:
            val expected1 = resultType match
              case None => Type.NoType()
              case Some(expected) => checkType(expected).!!
            //println(s"checkTerm $expr with new env")
            val expr1 = checkTerm(expr, expected = expected1).!!
            val captureSet = expr1.cv
            (expr1, Some(captureSet))
        case (ps: Syntax.TermParamList) :: pss =>
          checkTermParamList(ps.params).flatMap: params =>
            val params1 = params.map: param =>
              instantiateBinderCaps(param)
            go(pss)(using ctx.extend(params1)).map: (body1, captureSet) =>
              val tpe = Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star)
              val tpe1 = captureSet match
                case None => tpe
                case Some(cs) => 
                  val (_, cs1) = dropLocalParams(cs.elems, params.length)
                  Type.Capturing(tpe, CaptureSet(cs1))
              (Term.TermLambda(params, body1).withPosFrom(d).withTpe(tpe1).withCV(CaptureSet.empty), None)
        case (ps: Syntax.TypeParamList) :: pss =>
          checkTypeParamList(ps.params).flatMap: params =>
            go(pss)(using ctx.extend(params)).flatMap: (body1, captureSet) =>
              val tpe = Type.TypeArrow(params, body1.tpe).withKind(TypeKind.Star)
              val computeTpe1: Result[Type] = captureSet match
                case None => Right(tpe)
                case Some(cs) => 
                  val (existsLocalParams, cs1) = dropLocalParams(cs.elems, params.length)
                  if existsLocalParams then
                    Left(TypeError.GeneralError("local capture parameters captured by the body of a the lambda"))
                  else
                    Right(Type.Capturing(tpe, CaptureSet(cs1)))
              computeTpe1.map: tpe1 =>
                (Term.TypeLambda(params, body1).withPosFrom(d).withTpe(tpe1).withCV(CaptureSet.empty), None)
      go(paramss).flatMap: (expr1, captureSet) =>
        captureSet match
          case None =>
            val bd = TermBinder(name, expr1.tpe).withPos(d.pos)
            //println(s"final type = ${expr1.tpe}")
            Right((bd.asInstanceOf[TermBinder], expr1))
          case Some(cs) =>
            val tpe1 = Type.Capturing(Type.TypeArrow(Nil, expr1.tpe), cs)
            val expr2 = Term.TypeLambda(Nil, expr1).withPosFrom(d).withTpe(tpe1).withCV(CaptureSet.empty)
            val bd = TermBinder(name, tpe1).withPos(d.pos)
            Right((bd.asInstanceOf[TermBinder], expr2))

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
      // Create symbols for all definitions
      val syms = defns.map: defn =>
        defn match
          case _: (Syntax.Definition.ValDef | Syntax.Definition.DefDef) => 
            DefSymbol(defn.name, Definitions.anyType, mod).withPosFrom(defn)
          case _: Syntax.Definition.StructDef =>
            StructSymbol(defn.name, StructInfo(Nil, Nil), mod).withPosFrom(defn)
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

      hopefully:
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
        // Assign declared types to value definitions
        for ((sym, defn) <- syms `zip` defns) do
          (sym, defn) match
            case (sym: DefSymbol, defn: Syntax.Definition.ValDef) =>
              val defnType = extractDefnType(defn)(using ctxWithClasses).!!
              sym.tpe = defnType
            case (sym: DefSymbol, defn: Syntax.Definition.DefDef) =>
              val defnType = extractDefnType(defn)(using ctxWithClasses).!!
              sym.tpe = defnType
            case _ =>
        // Typecheck value definitions
        val valueDefTodos: List[(DefSymbol, Syntax.ValueDef)] = (syms `zip` defns).flatMap:
          case (sym: DefSymbol, defn: Syntax.ValueDef) => Some((sym, defn))
          case _ => None
        val valueDefns = checkDefns(valueDefTodos).!!
        // Done
        mod.defns = structDefns ++ valueDefns
        mod

  def isStablePath(t: Term): Boolean = t match
    case Term.Select(base, fieldInfo) => isStablePath(base) && !fieldInfo.mutable
    case Term.SymbolRef(_) => true
    case Term.BinderRef(_) => true
    case _ => false

  def checkSelect(base: Syntax.Term, field: String, srcPos: SourcePos)(using Context): Result[Term] =
    hopefully:
      val base1 = checkTerm(base).!!
      base1.tpe.stripCaptures match
        case PrimArrayType(elemType) =>
          field match
            case "size" | "length" =>
              Term.PrimOp(PrimitiveOp.ArrayLen, Nil, List(base1)).withPos(srcPos).withTpe(Definitions.i32Type).withCV(base1.cv)
            case _ => 
              sorry(TypeError.GeneralError(s"Field $field not found in `array`").withPos(srcPos))
        case _ => 
          getFieldInfo(base1.tpe, field) match
            case Some(fieldInfo) =>
              val outTerm = Term.Select(base1, fieldInfo).withPos(srcPos).withTpe(fieldInfo.tpe).withCV(base1.cv)
              if isStablePath(outTerm) then
                //println(s"$outTerm is a stable path!")
                val singletonSet = (outTerm.asInstanceOf[VarRef]).asSingletonType.singletonCaptureSet
                val narrowedType = Type.Capturing(outTerm.tpe.stripCaptures, singletonSet)
                outTerm.withTpe(narrowedType).withCV(singletonSet)
              else outTerm
            case None =>
              sorry(TypeError.TypeMismatch(s"a type with the field $field", base1.tpe.show).withPosFrom(base))

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
