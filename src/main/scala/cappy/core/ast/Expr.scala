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
    case Star
    case Tycon(arity: Int)

  enum Type extends Positioned:
    case Base(base: BaseType)
    case BinderRef(idx: Int)
    case Capturing(inner: Type, captureSet: CaptureSet)

  case class TermBinder(name: String, tpe: Type) extends Positioned
  case class TypeBinder(name: String, bound: Type) extends Positioned
  case class CaptureBinder(name: String, bound: CaptureSet) extends Positioned

  enum Term extends Positioned, Typed:
    case BinderRef(idx: Int)
    case StrLit(value: String)
    case IntLit(value: Int)
    case UnitLit()
    case TermLambda(params: List[TermBinder], body: Term)
    case TypeLambda(params: List[TypeBinder | CaptureBinder], body: Term)
