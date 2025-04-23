package cavia
package typechecking

import core.*
import ast.*
import scala.collection.mutable.ArrayBuffer

object TypeChecker:
  import Expr.*
  import Binder.*

  case class CaptureEnv(var cv: Set[CaptureRef], level: Int):
    def add(ref: CaptureRef): Unit = 
      cv += ref

    def add(refs: Set[CaptureRef]): Unit =
      cv ++= refs

  object CaptureEnv:
    def empty(level: Int): CaptureEnv = CaptureEnv(Set.empty, level)

  /** Type checking context. */
  case class Context(binders: List[Binder], symbols: List[Symbol], captured: CaptureEnv):
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

    def withEnv(env: CaptureEnv): Context =
      copy(captured = env)

  object Context:
    def empty: Context = Context(Nil, Nil, CaptureEnv.empty(level = 0))

  enum TypeError extends Positioned:
    case UnboundVariable(name: String, addenda: String = "")
    case TypeMismatch(expected: String, actual: String)
    case LeakingLocalBinder(tp: String)
    case GeneralError(msg: String)

    override def toString(): String = this match
      case UnboundVariable(name, addenda) => s"Unbound variable: $name$addenda"
      case TypeMismatch(expected, actual) => s"Type mismatch: expected $expected, but got $actual"
      case LeakingLocalBinder(tp) => s"Leaking local binder: $tp"
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

  def getBinder(idx: Int)(using ctx: Context): Binder =
    assert(idx >= 0 && idx < ctx.binders.length, s"invalid binder index: $idx")
    val bd = ctx.binders(idx)
    bd.shift(idx + 1)

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
      case Some(sym: DefSymbol) => Right(CaptureRef.Ref(Term.SymbolRef(sym)).maybeWithPosFrom(ref))
      case Some((binder: (Binder.CaptureBinder | Binder.TermBinder), idx)) => Right(CaptureRef.Ref(Term.BinderRef(idx)).maybeWithPosFrom(ref))
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

  def findBaseType(name: String): Option[BaseType] = name match
    case "Unit" => Some(BaseType.UnitType)
    case "Int" => Some(BaseType.IntType)
    case "String" => Some(BaseType.StrType)
    case "Any" => Some(BaseType.AnyType)
    case "i32" => Some(BaseType.I32)
    case "i64" => Some(BaseType.I64)
    case "f32" => Some(BaseType.F32)
    case "f64" => Some(BaseType.F64)
    case _ => None
    
  def checkType(tpe: Syntax.Type)(using Context): Result[Type] = tpe match
    case Syntax.Type.Ident(name) => 
      findBaseType(name) match
        case Some(baseType) => Right(Type.Base(baseType).withKind(TypeKind.Star).withPos(tpe.pos))
        case None => lookupBinder(name) match
          case Some((binder: Binder.TypeBinder, idx)) => Right(Type.BinderRef(idx).withKind(TypeKind.Star).maybeWithPosFrom(tpe))
          case Some((binder: Binder, idx)) => Left(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a type").maybeWithPosFrom(tpe))
          case _ => Left(TypeError.UnboundVariable(name).withPos(tpe.pos))
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
      for
        inner1 <- checkType(inner)
        captureSet1 <- checkCaptureSet(captureSet)
      yield
        Type.Capturing(inner1, captureSet1).maybeWithPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.AppliedType(tycon, args) => ???

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

  def markFree(cs: CaptureSet)(using Context): Unit =
    val crefs = cs.elems.flatMap: ref =>
      ref match
        case CaptureRef.Ref(ref) => Some(ref)
        case CaptureRef.CAP() => None
    crefs.foreach(markFree)

  def markFree(ref: VarRef)(using Context): Unit =
    def widenUntilWf(crefs: List[CaptureRef], delta: Int): List[CaptureRef] =
      def allWf(crefs: List[CaptureRef]): Boolean = crefs.forall: ref =>
        ref match
          case CaptureRef.Ref(Term.BinderRef(idx)) => idx >= delta
          case _ => true
      if allWf(crefs) then
        crefs.map: ref =>
          ref match
            case CaptureRef.Ref(Term.BinderRef(idx)) =>
              //assert(idx >= delta)
              CaptureRef.Ref(Term.BinderRef(idx - delta))
            case _ => ref
      else
        val crefs1 = crefs.flatMap: ref =>
          ref match
            case CaptureRef.Ref(Term.BinderRef(idx)) if idx < delta =>
              getBinder(idx) match
                case TermBinder(name, tpe) => tpe.captureSet.elems
                case CaptureBinder(name, bound) => bound.elems
                case _ => assert(false)
            case ref => List(ref)
        widenUntilWf(crefs1, delta)

    def avoidVarRef(ref: VarRef, delta: Int): List[CaptureRef] = ref match
      case Term.BinderRef(idx) => 
        if idx >= delta then
          List(Term.BinderRef(idx - delta).asCaptureRef)
        else
          //println(s"!!! markFree: dropping local binder $ref")
          widenUntilWf(List(ref.asCaptureRef), delta)
      case Term.SymbolRef(sym) => List(ref.asCaptureRef)
    val level = ctx.captured.level
    val curLevel = ctx.binders.length
    val delta = curLevel - level
    assert(delta >= 0, s"absurd levels")
    val crefs = avoidVarRef(ref, delta)
    //println(s"markFree $ref ==> $crefs")
    ctx.captured.add(crefs.toSet)

  private def dropLocalParams(crefs: List[CaptureRef], numParams: Int): List[CaptureRef] = crefs.flatMap: ref =>
    ref match
      case CaptureRef.Ref(Term.BinderRef(idx)) =>
        if idx >= numParams then
          Some(CaptureRef.Ref(Term.BinderRef(idx - numParams)).maybeWithPosFrom(ref))
        else None
      case _ => Some(ref)

  def checkTerm(t: Syntax.Term, expected: Type = Type.NoType)(using Context): Result[Term] = 
    val result: Result[Term] = t match
      case Syntax.Term.Ident(name) => 
        hopefully:
          val (ref, tpe) = lookupAll(name) match
            case Some(sym: DefSymbol) => (Term.SymbolRef(sym): VarRef, sym.tpe)
            case Some((binder: Binder.TermBinder, idx)) => (Term.BinderRef(idx): VarRef, binder.tpe)
            case Some((binder: Binder, idx)) => sorry(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a term").withPos(t.pos))
            case _ => sorry(TypeError.UnboundVariable(name).withPos(t.pos))
          if !tpe.isPure then markFree(ref)
          tpe.stripCaptures match
            case LazyType(resultType) => 
              val t1 = Term.TypeApply(ref, Nil).withPosFrom(t)
              t1.withTpe(resultType)
            case _ =>
              val tpe1 = if tpe.isPure then tpe else Type.Capturing(tpe.stripCaptures, ref.singletonCaptureSet).withKind(TypeKind.Star)
              ref.withPosFrom(t).withTpe(tpe1)
      case Syntax.Term.StrLit(value) => 
        Right(Term.StrLit(value).withPosFrom(t).withTpe(Definitions.strType))
      case Syntax.Term.IntLit(value) => 
        val tpe = if expected.exists && expected.isIntegralType then expected else Definitions.i32Type
        Right(Term.IntLit(value).withPosFrom(t).withTpe(tpe))
      case Syntax.Term.UnitLit() => 
        Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType))
      case Syntax.Term.Lambda(params, body) => 
        checkTermParamList(params).flatMap: params =>
          val ctx1 = ctx.extend(params)
          val env1 = CaptureEnv.empty(ctx1.binders.length)
          checkTerm(body)(using ctx1.withEnv(env1)).map: body1 =>
            val t1 = Term.TermLambda(params, body1).withPosFrom(t)
            val cv = env1.cv
            val cv1 = dropLocalParams(cv.toList, params.length)
            val tpe = Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star)
            val tpe1 = Type.Capturing(tpe.stripCaptures, CaptureSet(cv1)).withKind(TypeKind.Star)
            t1.withTpe(tpe1)
      case Syntax.Term.TypeLambda(params, body) =>
        checkTypeParamList(params).flatMap: params =>
          val ctx1 = ctx.extend(params)
          val env1 = CaptureEnv.empty(ctx1.binders.length)
          checkTerm(body)(using ctx1.withEnv(env1)).map: body1 =>
            val t1 = Term.TypeLambda(params, body1).withPosFrom(t)
            val cv = env1.cv
            val cv1 = dropLocalParams(cv.toList, params.length)
            // TODO: If it is a type lambda, then local parameter must not be used in the body
            // Or maybe, have two different kinds of capture lambdas, one charging its arguments, one not
            val tpe = Type.TypeArrow(params, body1.tpe).withKind(TypeKind.Star)
            val tpe1 = Type.Capturing(tpe.stripCaptures, CaptureSet(cv1)).withKind(TypeKind.Star)
            t1.withTpe(tpe1)
      case Syntax.Term.Block(stmts) => 
        def avoidSelfType(tpe: Type, d: Syntax.Definition): Result[Type] =
          val tm = AvoidLocalBinder(CaptureSet.universal)
          val result = tm.apply(tpe)
          hopefully:
            if tm.ok then result else sorry(TypeError.GeneralError(s"Cannot avoid self type").withPos(d.pos))
        def go(stmts: List[Syntax.Definition | Syntax.Term])(using Context): Result[Term] = 
          def goDefinition(d: Syntax.Definition)(using Context): Result[(TermBinder, Term, Boolean)] =
            hopefully:
              val selfType = d match
                case _: Syntax.Definition.ValDef => None
                case d: Syntax.Definition.DefDef => Some(extractDefType(d, requireExplictType = false).!!)
              val ctx1 = selfType match
                case None => ctx
                case Some(selfType) =>
                  val selfBinder = TermBinder(d.name, selfType).withPos(d.pos)
                  ctx.extend(selfBinder)
              val (bd, expr) = checkDef(d.asInstanceOf[Syntax.ValueDef])(using ctx1).!!
              val bd1 = 
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(bd.tpe, d).!!
                  TermBinder(bd.name, tpe1).withPos(bd.pos).asInstanceOf[TermBinder]
                else bd
              val expr1 =
                if selfType.isDefined then
                  val tpe1 = avoidSelfType(expr.tpe, d).!!
                  expr.withTpe(tpe1)
                else expr
              (bd1, expr1, selfType.isDefined)
          stmts match
            case Nil => 
              Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType))
            case (t: Syntax.Term) :: Nil => 
              checkTerm(t)
            case (d: Syntax.Definition) :: Nil =>
              hopefully:
                val (bd1, expr1, isRecursive) = goDefinition(d).!!
                val retType = Definitions.unitType
                Term.Bind(bd1, recursive = isRecursive, expr1, Term.UnitLit().withPosFrom(d).withTpe(retType)).withPosFrom(d).withTpe(retType)
            case d :: ds =>
              val d1 = d match
                case d: Syntax.Definition => d
                case t: Syntax.Term => Syntax.Definition.ValDef(Fresh.freshName("_"), None, t).withPosFrom(t)
              hopefully:
                val (bd1, boundExpr1, isRecursive) = goDefinition(d1).!!
                val bodyExpr = go(ds)(using ctx.extend(bd1 :: Nil)).!!
                val resType = bodyExpr.tpe
                val tm = AvoidLocalBinder(bd1.tpe.captureSet)
                val resType1 = tm.apply(resType)
                if tm.ok then
                  Term.Bind(bd1, recursive = isRecursive, boundExpr1, bodyExpr).withPosFrom(d, bodyExpr).withTpe(resType1)
                else
                  sorry(TypeError.LeakingLocalBinder(resType.show(using ctx.extend(bd1 :: Nil))).withPos(d.pos))
        go(stmts)
      case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if PrimitiveOp.fromName(name).isDefined => 
        PrimitiveOp.fromName(name).get match
          case PrimitiveOp.I32Add => checkPrimOpArgs(PrimitiveOp.I32Add, args, List(BaseType.I32, BaseType.I32), BaseType.I32, t.pos)
          case PrimitiveOp.I32Mul => checkPrimOpArgs(PrimitiveOp.I32Mul, args, List(BaseType.I32, BaseType.I32), BaseType.I32, t.pos)
          case PrimitiveOp.I64Add => checkPrimOpArgs(PrimitiveOp.I64Add, args, List(BaseType.I64, BaseType.I64), BaseType.I64, t.pos)
          case PrimitiveOp.I64Mul => checkPrimOpArgs(PrimitiveOp.I64Mul, args, List(BaseType.I64, BaseType.I64), BaseType.I64, t.pos)
          case PrimitiveOp.I32Println => checkPrimOpArgs(PrimitiveOp.I32Println, args, List(BaseType.I32), BaseType.UnitType, t.pos)
          case PrimitiveOp.I32Read => checkPrimOpArgs(PrimitiveOp.I32Read, args, List(), BaseType.I32, t.pos)
          case PrimitiveOp.Sorry =>
            hopefully:
              if expected.exists then
                Term.PrimOp(PrimitiveOp.Sorry, Nil).withPosFrom(t).withTpe(expected)
              else sorry(TypeError.GeneralError("no expected type for sorry").withPos(t.pos))
      case Syntax.Term.Apply(fun, args) => 
        checkTerm(fun).flatMap: fun1 =>
          val funType = fun1.tpe
          funType.stripCaptures match
            case Type.TermArrow(formals, resultType) =>
              if args.length != formals.length then
                Left(TypeError.GeneralError(s"Argument number mismatch, expected ${formals.length}, but got ${args.length}").withPos(t.pos))
              else
                def go(xs: List[(Syntax.Term, Type)], acc: List[Term]): Result[Term] = xs match
                  case Nil =>
                    val args = acc.reverse
                    substituteAll(resultType, args).map: resultType1 =>
                      Term.Apply(fun1, args).withPosFrom(t).withTpe(resultType1)
                  case (arg, formal) :: xs => 
                    checkTerm(arg, expected = formal).flatMap: arg1 =>
                      def mxs1: Result[List[(Syntax.Term, Type)]] = hopefully:
                        xs.zipWithIndex.map: 
                          case ((arg, formal), idx) =>
                            (arg, substitute(formal, arg1, idx, isParamType = true).!!)
                      mxs1.flatMap: xs1 =>
                        go(xs1, arg1 :: acc)
                go(args `zip` (formals.map(_.tpe)), Nil).map: resultTerm =>
                  fun1 match
                    case _: VarRef =>
                    case _ =>
                      markFree(resultTerm.tpe.captureSet)
                  resultTerm
            case _ => Left(TypeError.GeneralError(s"Expected a function, but got ${funType.show}").withPos(t.pos))
      case Syntax.Term.TypeApply(term, targs) => 
        hopefully:
          val term1 = checkTerm(term).!!
          term1.tpe.stripCaptures match
            case Type.TypeArrow(formals, resultType) => 
              val formalTypes: List[(Type | CaptureSet)] = formals.map:
                case bd: TypeBinder => bd.bound
                case bd: CaptureBinder => bd.bound
              def go(xs: List[((Syntax.Type | Syntax.CaptureSet), (Type | CaptureSet))], acc: List[(Type | CaptureSet)]): Term = xs match
                case Nil =>
                  val targs = acc.reverse
                  val resultType1 = substituteAllType(resultType, targs)
                  Term.TypeApply(term1, targs).withPosFrom(t).withTpe(resultType1)
                case (targ, tformal) :: xs => 
                  val targ1: Type | CaptureSet = 
                    (targ, tformal) match
                      case (targ: Syntax.Type, tformal: Type) => 
                        val targ1 = checkType(targ).!!
                        if TypeComparer.checkSubtype(targ1, tformal) then
                          targ1
                        else 
                          sorry(TypeError.TypeMismatch(tformal.show, targ1.show).withPos(targ.pos))
                      case (targ: Syntax.CaptureSet, tformal: CaptureSet) => 
                        val targ1 = checkCaptureSet(targ).!!
                        if TypeComparer.checkSubcapture(targ1, tformal) then
                          targ1
                        else
                          sorry(TypeError.TypeMismatch(tformal.show, targ1.show).withPos(targ.pos))
                      case _ => sorry(TypeError.GeneralError(s"argument kind mismatch").withPos(targ.pos))
                  val xs1 = xs.zipWithIndex.map: 
                    case ((targ, tformal), idx) =>
                      (targ, substituteType(tformal, targ1, idx, isParamType = true))
                  go(xs1, targ1 :: acc)
              val resultTerm = go(targs `zip` formalTypes, Nil)
              term1 match
                case _: VarRef =>
                case _ =>
                  markFree(resultTerm.tpe.captureSet)
              resultTerm
            case _ => 
              sorry(TypeError.GeneralError(s"Expected a function, but got $term1.tpe.show").withPos(t.pos))

    result.flatMap: t1 =>
      if !expected.exists || TypeComparer.checkSubtype(t1.tpe, expected) then
        val t2 = if expected.exists then t1.withTpe(expected) else t1
        Right(t2)
      else 
        Left(TypeError.TypeMismatch(expected.show, t1.tpe.show).withPos(t.pos))

  def substitute(tpe: Type, arg: Term, openingIdx: Int = 0, isParamType: Boolean = false): Result[Type] =
    val startingVariance = if isParamType then Variance.Contravariant else Variance.Covariant
    arg match
      case ref: VarRef =>
        val tm = OpenTermBinderExact(ref, openingIdx, startingVariance)
        Right(tm.apply(tpe))
      case _ => 
        val argType = arg.tpe
        val tm = OpenTermBinder(argType, openingIdx, startingVariance)
        val result = tm.apply(tpe)
        if tm.ok then
          Right(result)
        else
          Left(TypeError.GeneralError(s"Cannot substitute $arg into $tpe because the argument occurs in a invariant position").withPos(arg.pos))

  def substituteType[X <: Type | CaptureSet](tpe: X, arg: (Type | CaptureSet), openingIdx: Int = 0, isParamType: Boolean = false): X = 
    val tm = arg match
      case tpe: Type => OpenTypeBinder(tpe, openingIdx)
      case captureSet: CaptureSet => OpenCaptureBinder(captureSet, openingIdx)
    tpe match
      case tpe: Type => tm.apply(tpe).asInstanceOf[X]
      case captureSet: CaptureSet => tm.mapCaptureSet(captureSet).asInstanceOf[X]

  def substituteAll(tpe: Type, args: List[Term], openingIdx: Int = 0, isParamType: Boolean = false): Result[Type] =
    args match
      case Nil => Right(tpe)
      case arg :: args =>
        substitute(tpe, arg, openingIdx, isParamType).flatMap: tpe1 =>
          substituteAll(tpe1, args, openingIdx, isParamType)

  def substituteAllType(tpe: Type, args: List[(Type | CaptureSet)], openingIdx: Int = 0, isParamType: Boolean = false): Type =
    args match
      case Nil => tpe
      case arg :: args =>
        val tpe1 = substituteType(tpe, arg, openingIdx, isParamType)
        substituteAllType(tpe1, args, openingIdx, isParamType)

  def checkPrimOpArgs(op: PrimitiveOp, args: List[Syntax.Term], formals: List[BaseType], resType: BaseType, pos: SourcePos)(using Context): Result[Term] = 
    def go(args: List[Syntax.Term], formals: List[BaseType], acc: List[Term]): Result[List[Term]] = (args, formals) match
      case (Nil, Nil) => Right(acc.reverse)
      case (arg :: args, formal :: formals) =>
        checkTerm(arg, expected = Type.Base(formal).withKind(TypeKind.Star)).flatMap: arg1 =>
          go(args, formals, arg1 :: acc)
      case _ => Left(TypeError.GeneralError(s"Argument number mismatch for primitive operation, expected ${formals.length}, but got ${args.length}").withPos(pos))
    go(args, formals, Nil).map: args1 =>
      Term.PrimOp(op, args1).withPos(pos).withTpe(Type.Base(resType).withKind(TypeKind.Star))

  def checkDef(d: Syntax.ValueDef)(using Context): Result[(TermBinder, Term)] = d match
    case Syntax.Definition.ValDef(name, tpe, expr) =>
      hopefully:
        val expected1 = tpe match
          case None => Type.NoType
          case Some(expected) => checkType(expected).!!
        val expr1 = checkTerm(expr, expected = expected1).!!
        val binderType = if expected1.exists then expected1 else expr1.tpe
        val bd = TermBinder(name, binderType).withPos(d.pos)
        (bd.asInstanceOf[TermBinder], expr1)
    case Syntax.Definition.DefDef(name, _, paramss, resultType, expr) => 
      def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[(Term, Option[CaptureSet])] = pss match
        case Nil => 
          val env1 = CaptureEnv.empty(ctx.binders.length)
          hopefully:
            val expected1 = resultType match
              case None => Type.NoType
              case Some(expected) => checkType(expected).!!
            val expr1 = checkTerm(expr, expected = expected1)(using ctx.withEnv(env1)).!!
            val captureSet = CaptureSet(env1.cv.toList)
            (expr1, Some(captureSet))
        case (ps: Syntax.TermParamList) :: pss =>
          checkTermParamList(ps.params).flatMap: params =>
            go(pss)(using ctx.extend(params)).map: (body1, captureSet) =>
              val tpe = Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star)
              val tpe1 = captureSet match
                case None => tpe
                case Some(cs) => Type.Capturing(tpe, cs)
              (Term.TermLambda(params, body1).withPosFrom(d).withTpe(tpe1), None)
        case (ps: Syntax.TypeParamList) :: pss =>
          checkTypeParamList(ps.params).flatMap: params =>
            go(pss)(using ctx.extend(params)).map: (body1, captureSet) =>
              val tpe = Type.TypeArrow(params, body1.tpe).withKind(TypeKind.Star)
              val tpe1 = captureSet match
                case None => tpe
                case Some(cs) => Type.Capturing(tpe, cs)
              (Term.TypeLambda(params, body1).withPosFrom(d).withTpe(tpe1), None)
      go(paramss).flatMap: (expr1, captureSet) =>
        captureSet match
          case None =>
            val bd = TermBinder(name, expr1.tpe).withPos(d.pos)
            Right((bd.asInstanceOf[TermBinder], expr1))
          case Some(cs) =>
            val tpe1 = Type.Capturing(Type.TypeArrow(Nil, expr1.tpe), cs)
            val expr2 = Term.TypeLambda(Nil, expr1).withPosFrom(d).withTpe(tpe1)
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

  def extractDefnType(d: Syntax.Definition)(using Context): Result[Type] = d match
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
      val syms = defns.map: defn =>
        DefSymbol(defn.name, Definitions.anyType, mod).withPosFrom(defn)
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
              val d = Expr.Definition.ValDef(sym, expr)
              d :: defns1
      hopefully:
        for (sym, defn) <- syms `zip` defns do
          val defnType = extractDefnType(defn).!!
          sym.tpe = defnType
        val valueDefTodos: List[(DefSymbol, Syntax.ValueDef)] = (syms `zip` defns).flatMap:
          case (sym, defn: (Syntax.ValueDef)) => Some((sym, defn))
          case _ => None
        val defns1 = checkDefns(valueDefTodos).!!
        mod.defns = defns1
        mod
