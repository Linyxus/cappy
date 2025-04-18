package cappy
package core
package ast

object Expr:
  /** A trait for expressions that have a type */
  trait Typed:
    private var myTpe: Type | Null = null

    /** The type of the expression, assuming it is typed */
    def tpe: Type = myTpe.nn

    /** Whether the expression is typed */
    def typed: Boolean = myTpe != null

    /** Set the type of the expression */
    def setTpe(tpe: Type): Unit = myTpe = tpe

    /** Set the type of the expression and return self */
    def withTpe(tpe: Type): this.type =
      setTpe(tpe)
      this

  enum BaseType:
    case StrType
    case IntType
    case UnitType
    case AnyType

  enum CaptureRef extends Positioned:
    case BinderRef(idx: Int)
    case CAP()

  case class CaptureSet(elems: List[CaptureRef]) extends Positioned:
    def ++ (other: CaptureSet): CaptureSet = CaptureSet(elems ++ other.elems)

  object CaptureSet:
    def empty: CaptureSet = CaptureSet(Nil)
    def universal: CaptureSet = CaptureSet(List(CaptureRef.CAP()))

  enum TypeKind:
    case Star  // *
    case Arrow(arity: Int, res: TypeKind)  // [T1, T2, ...] -> K

  /** A trait for types that have a kind */
  trait HasKind:
    private var myKind: TypeKind | Null = null

    /** The kind of the type */
    def kind: TypeKind = myKind.nn

    /** Whether the type has a kind */
    def hasKind: Boolean = myKind != null

    /** Set the kind of the type */
    def setKind(kind: TypeKind): Unit = myKind = kind

    /** Set the kind of the type and return self */
    def withKind(kind: TypeKind): this.type =
      setKind(kind)
      this

  enum Binder extends Positioned:
    case TermBinder(name: String, tpe: Type)
    case TypeBinder(name: String, bound: Type)
    case CaptureBinder(name: String, bound: CaptureSet)

    val name: String

    def kindStr: String = this match
      case TermBinder(_, _) => "term"
      case TypeBinder(_, _) => "type"
      case CaptureBinder(_, _) => "capture"

  import Binder.*

  enum Type extends Positioned, HasKind:
    case Base(base: BaseType)
    case BinderRef(idx: Int)
    case Capturing(inner: Type, captureSet: CaptureSet)
    case TermArrow(params: List[TermBinder], result: Type)
    case TypeArrow(params: List[TypeBinder | CaptureBinder], result: Type)

    def like(other: Type): this.type =
      assert(other.hasKind, s"Type $other does not have a kind when calling like")
      if other.hasPos then
        this.withPosFrom(other)
      this.withKind(other.kind)

  enum Term extends Positioned, Typed:
    case BinderRef(idx: Int)
    case StrLit(value: String)
    case IntLit(value: Int)
    case UnitLit()
    case TermLambda(params: List[TermBinder], body: Term)
    case TypeLambda(params: List[TypeBinder | CaptureBinder], body: Term)
    case Bind(binder: TermBinder, bound: Term, body: Term)

  object Definitions:
    def anyType: Type = Type.Base(BaseType.AnyType).withKind(TypeKind.Star)
    def strType: Type = Type.Base(BaseType.StrType).withKind(TypeKind.Star)
    def intType: Type = Type.Base(BaseType.IntType).withKind(TypeKind.Star)
    def unitType: Type = Type.Base(BaseType.UnitType).withKind(TypeKind.Star)
    def capCaptureSet: CaptureSet = CaptureSet(List(CaptureRef.CAP()))

  class TypeMap:
    var localBinders: List[Binder] = Nil

    def withBinder[T](bd: Binder)(op: => T): T =
      localBinders = bd :: localBinders
      try op finally localBinders = localBinders.tail

    def apply(tp: Type): Type = mapOver(tp)

    def mapBinder(param: Binder): Binder = param match
      case TermBinder(name, tpe) => TermBinder(name, apply(tpe)).withPosFrom(param)
      case TypeBinder(name, bound) => TypeBinder(name, apply(bound)).withPosFrom(param)
      case CaptureBinder(name, bound) => CaptureBinder(name, mapCaptureSet(bound)).withPosFrom(param)

    def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
      CaptureSet(captureSet.elems.map(mapCaptureRef)).withPosFrom(captureSet)

    def mapCaptureRef(ref: CaptureRef): CaptureRef = ref

    def mapOver(tp: Type): Type = tp match
      case Type.Capturing(inner, captureSet) =>
        Type.Capturing(apply(inner), mapCaptureSet(captureSet)).like(tp)
      case Type.TermArrow(params, result) => 
        def go(ps: List[TermBinder], bs: List[Binder]): Type =
          ps match
            case Nil => Type.TermArrow(bs.reverse.asInstanceOf, apply(result)).like(tp)
            case p :: ps =>
              val p1 = mapBinder(p)
              withBinder(p1):
                go(ps, p1 :: bs)
        go(params, Nil)
      case Type.TypeArrow(params, result) =>
        def go(ps: List[TypeBinder | CaptureBinder], bs: List[Binder]): Type =
          ps match
            case Nil => Type.TypeArrow(bs.reverse.asInstanceOf, apply(result)).like(tp)
            case p :: ps =>
              val p1 = mapBinder(p)
              withBinder(p1):
                go(ps, p1 :: bs)
        go(params, Nil)
      case _ => tp

  class ShiftType(amount: Int) extends TypeMap:
    override def mapCaptureRef(ref: CaptureRef): CaptureRef = ref match
      case CaptureRef.BinderRef(idx) if idx >= localBinders.size =>
        CaptureRef.BinderRef(idx + amount).withPosFrom(ref)
      case ref => ref

  extension (tpe: Type)
    def shift(amount: Int): Type =
      val shift = ShiftType(amount)
      val result = shift(tpe)
      assert(result.hasPos == tpe.hasPos && result.hasKind)
      result

  extension (captureSet: CaptureSet)
    def shift(amount: Int): CaptureSet =
      val shifter = ShiftType(amount)
      val result = shifter.mapCaptureSet(captureSet)
      assert(result.hasPos == captureSet.hasPos)
      result

  extension (binder: Binder)
    def shift(amount: Int): Binder =
      binder match
        case TermBinder(name, tpe) => TermBinder(name, tpe.shift(amount)).withPosFrom(binder)
        case TypeBinder(name, bound) => TypeBinder(name, bound.shift(amount)).withPosFrom(binder)
        case CaptureBinder(name, bound) => CaptureBinder(name, bound.shift(amount)).withPosFrom(binder)
