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

  case class CaptureSet(elems: List[CaptureRef]) extends Positioned

  enum TypeKind:
    case Star  // *
    case Arrow(arity: Int, res: TypeKind)  // [T1, T2, ...] -> K

  /** A trait for types that have a kind */
  trait HasKind:
    private var myKind: TypeKind | Null = null

    /** The kind of the type */
    def kind: TypeKind = myKind.nn

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

  import Binder.*

  enum Type extends Positioned, HasKind:
    case Base(base: BaseType)
    case BinderRef(idx: Int)
    case Capturing(inner: Type, captureSet: CaptureSet)
    case TermArrow(params: List[TermBinder], result: Type)
    case TypeArrow(params: List[TypeBinder | CaptureBinder], result: Type)

  enum Term extends Positioned, Typed:
    case BinderRef(idx: Int)
    case StrLit(value: String)
    case IntLit(value: Int)
    case UnitLit()
    case TermLambda(params: List[TermBinder], body: Term)
    case TypeLambda(params: List[TypeBinder | CaptureBinder], body: Term)

  object Definitions:
    def anyType: Type = Type.Base(BaseType.AnyType).withKind(TypeKind.Star)
    def capCaptureSet: CaptureSet = CaptureSet(List(CaptureRef.CAP()))
