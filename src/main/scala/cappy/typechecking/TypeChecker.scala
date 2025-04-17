package cappy
package typechecking

import core.*
import core.ast.*

object TypeChecker:
  import Expr.*
  import Binder.*

  /** Type checking context. */
  case class Context(binders: List[Binder]):
    /** Extend the context with a list of binders. */
    def extend(bds: List[Binder]): Context =
      if bds.isEmpty then this
      else
        var newBinders = binders
        for bd <- bds do
          newBinders = bd :: newBinders
        copy(binders = newBinders)
  object Context:
    def empty: Context = Context(Nil)

  enum TypeError extends Positioned:
    case UnboundVariable(name: String, addenda: String = "")
    case TypeMismatch(expected: Type, actual: Type)

  def ctx(using myCtx: Context): Context = myCtx
  
  type Result[+A] = Either[TypeError, A]

  def lookupBinder(name: String)(using ctx: Context): Option[(Binder, Int)] =
    ctx.binders.zipWithIndex.find((binder, _) => binder.name == name).map: (bd, idx) =>
      (bd.shift(idx + 1), idx)

  def checkTermParam(param: Syntax.TermParam)(using ctx: Context): Result[TermBinder] =
    checkType(param.tpe).map: tpe =>
      val binder: TermBinder = TermBinder(param.name, tpe)
      binder.withPosFrom(param)

  def checkTypeParam(param: Syntax.TypeParam)(using ctx: Context): Result[TypeBinder] =
    val bound: Result[Type] = param.bound match
      case None => Right(Definitions.anyType.withPosFrom(param))
      case Some(tpe) => 
        // TODO: check that the bound is pure
        checkType(tpe)
    bound.map: tpe =>
      val binder: TypeBinder = TypeBinder(param.name, tpe)
      binder.withPosFrom(param)

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
      Right(CaptureRef.CAP().withPosFrom(ref))
    else lookupBinder(ref.name) match
      case Some((binder: (Binder.CaptureBinder | Binder.TermBinder), idx)) => Right(CaptureRef.BinderRef(idx).withPosFrom(ref))
      case Some((binder: Binder.TypeBinder, idx)) => Left(TypeError.UnboundVariable(ref.name, s"I found a type name, but was looking for either a term or capture name").withPos(ref.pos))
      case _ => Left(TypeError.UnboundVariable(ref.name).withPos(ref.pos))

  def checkCaptureSet(captureSet: Syntax.CaptureSet)(using ctx: Context): Result[CaptureSet] =
    val checkElems: List[Result[CaptureRef]] = captureSet.elems.map(checkCaptureRef)
    @annotation.tailrec
    def go(elems: List[Result[CaptureRef]], acc: List[CaptureRef]): Result[CaptureSet] = elems match
      case Nil => Right(CaptureSet(acc.reverse).withPosFrom(captureSet))
      case Left(error) :: _ => Left(error)
      case Right(elem) :: rest => go(rest, elem :: acc)
    go(checkElems, Nil)

  def findBaseType(name: String): Option[BaseType] = name match
    case "Unit" => Some(BaseType.UnitType)
    case "Int" => Some(BaseType.IntType)
    case "String" => Some(BaseType.StrType)
    case "Any" => Some(BaseType.AnyType)
    case _ => None
    
  def checkType(tpe: Syntax.Type)(using Context): Result[Type] = tpe match
    case Syntax.Type.Ident(name) => 
      findBaseType(name) match
        case Some(baseType) => Right(Type.Base(baseType).withKind(TypeKind.Star).withPos(tpe.pos))
        case None => lookupBinder(name) match
          case Some((binder: Binder.TypeBinder, idx)) => Right(Type.BinderRef(idx).withKind(TypeKind.Star).withPos(tpe.pos))
          case Some((binder: Binder, idx)) => Left(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a type").withPos(tpe.pos))
          case _ => Left(TypeError.UnboundVariable(name).withPos(tpe.pos))
    case Syntax.Type.Arrow(params, result) => 
      def go(ps: List[Syntax.TermParam], acc: List[TermBinder])(using Context): Result[List[TermBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkTermParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkType(result)(using ctx.extend(params)).map: result1 =>
          Type.TermArrow(params, result1).withPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.TypeArrow(params, result) => 
      def go(ps: List[Syntax.TypeParam | Syntax.CaptureParam], acc: List[TypeBinder | CaptureBinder])(using Context): Result[List[TypeBinder | CaptureBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkCaptureOrTypeParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkType(result)(using ctx.extend(params)).map: result1 =>
          Type.TypeArrow(params, result1).withPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.Capturing(inner, captureSet) =>
      for
        inner1 <- checkType(inner)
        captureSet1 <- checkCaptureSet(captureSet)
      yield
        Type.Capturing(inner1, captureSet1).withPosFrom(tpe).withKind(TypeKind.Star)
    case Syntax.Type.AppliedType(tycon, args) => ???

  def checkTerm(t: Syntax.Term)(using Context): Result[Term] = t match
    case Syntax.Term.Ident(name) => lookupBinder(name) match
      case Some((binder: Binder.TermBinder, idx)) => 
        val tpe = binder.tpe
        // TODO: deal with capture and reach refinements
        Right(Term.BinderRef(idx).withPosFrom(t).withTpe(tpe))
      case Some((binder: Binder, idx)) => Left(TypeError.UnboundVariable(name, s"I found a ${binder.kindStr} name, but was looking for a term").withPos(t.pos))
      case None => Left(TypeError.UnboundVariable(name).withPos(t.pos))
    case Syntax.Term.StrLit(value) => 
      Right(Term.StrLit(value).withPosFrom(t).withTpe(Definitions.strType))
    case Syntax.Term.IntLit(value) => 
      Right(Term.IntLit(value).withPosFrom(t).withTpe(Definitions.intType))
    case Syntax.Term.UnitLit() => 
      Right(Term.UnitLit().withPosFrom(t).withTpe(Definitions.unitType))
    case Syntax.Term.Lambda(params, body) => 
      def go(ps: List[Syntax.TermParam], acc: List[TermBinder])(using Context): Result[List[TermBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkTermParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkTerm(body)(using ctx.extend(params)).map: body1 =>
          val t1 = Term.TermLambda(params, body1).withPosFrom(t)
          val tpe = Type.TermArrow(params, body1.tpe)
          t1.withTpe(tpe)
    case Syntax.Term.TypeLambda(params, body) =>
      def go(ps: List[Syntax.TypeParam | Syntax.CaptureParam], acc: List[TypeBinder | CaptureBinder])(using Context): Result[List[TypeBinder | CaptureBinder]] = ps match
        case Nil => Right(acc.reverse)
        case p :: ps =>
          checkCaptureOrTypeParam(p).flatMap: binder =>
            go(ps, binder :: acc)(using ctx.extend(binder :: Nil))
      go(params, Nil).flatMap: params =>
        checkTerm(body)(using ctx.extend(params)).map: body1 =>
          val t1 = Term.TypeLambda(params, body1).withPosFrom(t)
          val tpe = Type.TypeArrow(params, body1.tpe)
          t1.withTpe(tpe)
    case Syntax.Term.Apply(fun, args) => ???
    case Syntax.Term.TypeApply(term, targs) => ???
    case Syntax.Term.CaptureApply(term, captures) => ???
    case Syntax.Term.Block(defs, expr) => ???
  
