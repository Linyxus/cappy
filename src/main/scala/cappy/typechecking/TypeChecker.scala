package cappy
package typechecking

import core.*
import core.ast.*
import cappy.core.ast.Syntax.Definition

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
      case Some(sym: Symbol) => Right(CaptureRef.SymbolRef(sym).maybeWithPosFrom(ref))
      case Some((binder: (Binder.CaptureBinder | Binder.TermBinder), idx)) => Right(CaptureRef.BinderRef(idx).maybeWithPosFrom(ref))
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
    def go(ps: List[Syntax.TermParam], acc: List[TermBinder])(using Context): Result[List[TermBinder]] = ps match
      case Nil => Right(acc.reverse)
      case p :: ps =>
        checkTermParam(p).flatMap: binder =>
          go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
    go(params, Nil)

  def checkTypeParamList(params: List[Syntax.TypeParam | Syntax.CaptureParam])(using Context): Result[List[TypeBinder | CaptureBinder]] =
    def go(ps: List[Syntax.TypeParam | Syntax.CaptureParam], acc: List[TypeBinder | CaptureBinder])(using Context): Result[List[TypeBinder | CaptureBinder]] = ps match
      case Nil => Right(acc.reverse)
      case p :: ps =>
        checkCaptureOrTypeParam(p).flatMap: binder =>
          go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
    go(params, Nil)

  def checkTerm(t: Syntax.Term)(using Context): Result[Term] = t match
    case Syntax.Term.Ident(name) => lookupAll(name) match
      case Some(sym: Symbol) => 
        val tpe = sym.tpe
        val ref: Term.SymbolRef = Term.SymbolRef(sym)
        val tpe1 = Type.Capturing(tpe.stripCaptures, ref.singletonCaptureSet).withKind(TypeKind.Star)
        Right(ref.withPosFrom(t).withTpe(tpe1))
      case Some((binder: Binder.TermBinder, idx)) => 
        val tpe = binder.tpe
        val ref: Term.BinderRef = Term.BinderRef(idx)
        val tpe1 = Type.Capturing(tpe.stripCaptures, ref.singletonCaptureSet).withKind(TypeKind.Star)
        Right(ref.withPosFrom(t).withTpe(tpe1))
      case Some((binder: Binder, idx)) => Left(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a term").withPos(t.pos))
      case None => Left(TypeError.UnboundVariable(name).withPos(t.pos))
    case Syntax.Term.StrLit(value) => 
      Right(Term.StrLit(value).withPosFrom(t).withTpe(Definitions.strType))
    case Syntax.Term.IntLit(value) => 
      Right(Term.IntLit(value).withPosFrom(t).withTpe(Definitions.i64Type))
    case Syntax.Term.UnitLit() => 
      Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType))
    case Syntax.Term.Lambda(params, body) => 
      checkTermParamList(params).flatMap: params =>
        checkTerm(body)(using ctx.extend(params)).map: body1 =>
          val t1 = Term.TermLambda(params, body1).withPosFrom(t)
          val tpe = Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star)
          t1.withTpe(tpe)
    case Syntax.Term.TypeLambda(params, body) =>
      checkTypeParamList(params).flatMap: params =>
        checkTerm(body)(using ctx.extend(params)).map: body1 =>
          val t1 = Term.TypeLambda(params, body1).withPosFrom(t)
          val tpe = Type.TypeArrow(params, body1.tpe)
          t1.withTpe(tpe)
    case Syntax.Term.Block(stmts) => 
      def go(stmts: List[Syntax.Definition | Syntax.Term])(using Context): Result[Term] = 
        stmts match
          case Nil => 
            Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType))
          case (t: Syntax.Term) :: Nil => 
            checkTerm(t)
          case (d: Syntax.Definition) :: Nil =>
            checkDef(d).map: (bd, expr) =>
              val retType = Definitions.unitType
              Term.Bind(bd, expr, Term.UnitLit().withPosFrom(d).withTpe(retType)).withPosFrom(d).withTpe(retType)
          case d :: ds =>
            val d1 = d match
              case d: Syntax.Definition => d
              case t: Syntax.Term => Syntax.Definition.ValDef(Fresh.freshName("_"), None, t).withPosFrom(t)
            checkDef(d1).flatMap: (bd, boundExpr) =>
              go(ds)(using ctx.extend(bd :: Nil)).flatMap: bodyExpr =>
                val resType = bodyExpr.tpe
                val tm = AvoidLocalBinder(bd.tpe.captureSet)
                val resType1 = tm.apply(resType)
                if tm.ok then
                  Right(Term.Bind(bd, boundExpr, bodyExpr).withPosFrom(d, bodyExpr).withTpe(resType1))
                else
                  Left(TypeError.LeakingLocalBinder(resType.show(using ctx.extend(bd))).withPos(d.pos))
      go(stmts)
    case Syntax.Term.Apply(Syntax.Term.Ident(name), args) if PrimitiveOp.fromName(name).isDefined => 
      PrimitiveOp.fromName(name).get match
        case PrimitiveOp.I32Add => checkPrimOpArgs(PrimitiveOp.I32Add, args, List(BaseType.I32, BaseType.I32), BaseType.I32, t.pos)
        case PrimitiveOp.I32Mul => checkPrimOpArgs(PrimitiveOp.I32Mul, args, List(BaseType.I32, BaseType.I32), BaseType.I32, t.pos)
        case PrimitiveOp.I64Add => checkPrimOpArgs(PrimitiveOp.I64Add, args, List(BaseType.I64, BaseType.I64), BaseType.I64, t.pos)
        case PrimitiveOp.I64Mul => checkPrimOpArgs(PrimitiveOp.I64Mul, args, List(BaseType.I64, BaseType.I64), BaseType.I64, t.pos)
    case Syntax.Term.Apply(fun, args) => ???
    case Syntax.Term.TypeApply(term, targs) => ???

  def checkPrimOpArgs(op: PrimitiveOp, args: List[Syntax.Term], formals: List[BaseType], resType: BaseType, pos: SourcePos)(using Context): Result[Term] = 
    def go(args: List[Syntax.Term], formals: List[BaseType], acc: List[Term]): Result[List[Term]] = (args, formals) match
      case (Nil, Nil) => Right(acc.reverse)
      case (arg :: args, formal :: formals) =>
        checkTerm(arg).flatMap: arg1 =>
          if TypeComparer.checkSubtype(arg1.tpe, Type.Base(formal).withKind(TypeKind.Star)) then
            go(args, formals, arg1 :: acc)
          else Left(TypeError.TypeMismatch(TypePrinter.show(formal), arg1.tpe.show).withPos(arg.pos))
      case _ => Left(TypeError.GeneralError(s"Argument number mismatch for primitive operation, expected ${formals.length}, but got ${args.length}").withPos(pos))
    go(args, formals, Nil).map: args1 =>
      Term.PrimOp(op, args1).withPos(pos).withTpe(Type.Base(resType).withKind(TypeKind.Star))

  def checkDef(d: Syntax.Definition)(using Context): Result[(TermBinder, Term)] = d match
    case Syntax.Definition.ValDef(name, tpe, expr) =>
      checkTerm(expr).flatMap: expr1 =>
        tpe match
          case None => 
            val bd = TermBinder(name, expr1.tpe).withPos(d.pos)
            Right((bd.asInstanceOf[TermBinder], expr1))
          case Some(expected) =>
            checkType(expected).flatMap: expected1 =>
              if TypeComparer.checkSubtype(expr1.tpe, expected1) then
                val bd = TermBinder(name, expected1).withPos(d.pos)
                Right((bd.asInstanceOf[TermBinder], expr1))
              else Left(TypeError.TypeMismatch(expected1.show, expr1.tpe.show).withPos(expected.pos))
    case Syntax.Definition.DefDef(name, paramss, resultType, expr) => 
      def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[Term] = pss match
        case Nil => checkTerm(expr).flatMap: expr1 =>
          resultType match
            case None => Right(expr1)
            case Some(expected) =>
              checkType(expected).flatMap: expected1 =>
                if TypeComparer.checkSubtype(expr1.tpe, expected1) then
                  Right(expr1.withTpe(expected1))
                else Left(TypeError.TypeMismatch(expected1.show, expr1.tpe.show).withPos(expected.pos))
        case (ps: Syntax.TermParamList) :: pss =>
          checkTermParamList(ps.params).flatMap: params =>
            go(pss)(using ctx.extend(params)).map: body1 =>
              Term.TermLambda(params, body1).withPosFrom(d).withTpe(Type.TermArrow(params, body1.tpe).withKind(TypeKind.Star))
        case (ps: Syntax.TypeParamList) :: pss =>
          checkTypeParamList(ps.params).flatMap: params =>
            go(pss)(using ctx.extend(params)).map: body1 =>
              Term.TypeLambda(params, body1).withPosFrom(d).withTpe(Type.TypeArrow(params, body1.tpe).withKind(TypeKind.Star))
      go(paramss).flatMap: expr1 =>
        val bd = TermBinder(name, expr1.tpe).withPos(d.pos)
        Right((bd.asInstanceOf[TermBinder], expr1))

  def extractDefType(d: Syntax.Definition.DefDef)(using Context): Result[Type] = 
    def go(pss: List[Syntax.TermParamList | Syntax.TypeParamList])(using Context): Result[Type] = pss match
      case Nil => 
        d.resultType match
          case None => Left(TypeError.GeneralError("Explicit type annotation is required for top-level definitions").withPos(d.pos))
          case Some(resultType) => checkType(resultType)
      case (ps: Syntax.TermParamList) :: pss =>
        checkTermParamList(ps.params).flatMap: params =>
          go(pss)(using ctx.extend(params)).flatMap: resultType1 =>
            Right(Type.TermArrow(params, resultType1).withKind(TypeKind.Star))
      case (ps: Syntax.TypeParamList) :: pss =>
        checkTypeParamList(ps.params).flatMap: params =>
          go(pss)(using ctx.extend(params)).flatMap: resultType1 =>
            Right(Type.TypeArrow(params, resultType1).withKind(TypeKind.Star))
    go(d.paramss)

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
      def extractSymTypes(xs: List[(Symbol, Syntax.Definition)]): Result[Unit] = xs match
        case Nil => Right(())
        case (sym, defn) :: xs =>
          extractDefnType(defn).flatMap: tpe =>
            sym.tpe = tpe
            extractSymTypes(xs)
      val syms = defns.map: defn =>
        Symbol(defn.name, Definitions.anyType, mod).withPosFrom(defn)
      extractSymTypes(syms `zip` defns)
      val ctx1 = ctx.addSymbols(syms)
      def checkDefns(defns: List[(Symbol, Syntax.Definition)]): Result[List[Expr.Definition]] = defns match
        case Nil => Right(Nil)
        case (sym, defn) :: defns =>
          checkDef(defn)(using ctx1).flatMap: (bd, expr) =>
            checkDefns(defns).map: defns1 =>
              val d = Expr.Definition.ValDef(sym, expr.tpe, expr)
              d :: defns1
      checkDefns(syms `zip` defns).map: defns1 =>
        mod.defns = defns1
        mod
  
