package cavia
package core
package ast

object Expr:
  /** A trait for expressions that have a type */
  trait Typed:
    private var myTpe: Type = Type.NoType

    /** The type of the expression, assuming it is typed */
    def tpe: Type = myTpe

    /** Whether the expression is typed */
    def typed: Boolean = myTpe.exists

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
    /** Primitive types in WebAssembly */
    case I32
    case I64
    case F32
    case F64

  enum CaptureRef extends Positioned:
    case Ref(ref: VarRef)
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
    /** By default, all types have the kind `*` */
    private var myKind: TypeKind = TypeKind.Star

    /** The kind of the type */
    def kind: TypeKind = myKind

    /** Whether the type has a kind */
    def hasKind: Boolean = true

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
    case NoType

    def like(other: Type): this.type =
      assert(other.hasKind, s"Type $other (id=${other.id}) does not have a kind when calling like")
      if other.hasPos then
        this.withPosFrom(other)
      this.withKind(other.kind)

    val id: Int =
      Type.nextId += 1
      //assert(Type.nextId != 20, "Gotcha!")  // for debugging
      Type.nextId

    def exists: Boolean = this match
      case NoType => false
      case _ => true
    
  
  object Type:
    private var nextId: Int = 0

  enum PrimitiveOp extends Positioned:
    case I32Add
    case I32Mul
    case I64Add
    case I64Mul

    override def toString: String = this match
      case I32Add => "#i32add"
      case I32Mul => "#i32mul"
      case I64Add => "#i64add"
      case I64Mul => "#i64mul"

  object PrimitiveOp:
    def fromName(name: String): Option[PrimitiveOp] = name match
      case "#i32add" => Some(PrimitiveOp.I32Add)
      case "#i32mul" => Some(PrimitiveOp.I32Mul)
      case "#i64add" => Some(PrimitiveOp.I64Add)
      case "#i64mul" => Some(PrimitiveOp.I64Mul)
      case _ => None

  enum Term extends Positioned, Typed:
    case BinderRef(idx: Int)
    case SymbolRef(sym: Symbol)
    case StrLit(value: String)
    case IntLit(value: Int)
    case UnitLit()
    case TermLambda(params: List[TermBinder], body: Term)
    case TypeLambda(params: List[TypeBinder | CaptureBinder], body: Term)
    case Bind(binder: TermBinder, bound: Term, body: Term)
    case PrimOp(op: PrimitiveOp, args: List[Term])
    case Apply(fun: Term, args: List[Term])

  /** Reference to a variable, either a binder or a symbol */
  type VarRef = Term.BinderRef | Term.SymbolRef

  case class Symbol(name: String, var tpe: Type, from: Module) extends Positioned
  enum Definition extends Positioned:
    case ValDef(sym: Symbol, tpe: Type, body: Term)
  class Module(var defns: List[Definition])

  object Definitions:
    def anyType: Type = Type.Base(BaseType.AnyType).withKind(TypeKind.Star)
    def strType: Type = Type.Base(BaseType.StrType).withKind(TypeKind.Star)
    def intType: Type = Type.Base(BaseType.IntType).withKind(TypeKind.Star)
    def i64Type: Type = Type.Base(BaseType.I64).withKind(TypeKind.Star)
    def unitType: Type = Type.Base(BaseType.UnitType).withKind(TypeKind.Star)
    def capCaptureSet: CaptureSet = CaptureSet(List(CaptureRef.CAP()))

  enum Variance:
    case Covariant
    case Contravariant
    case Invariant

    def negate: Variance = this match
      case Covariant => Contravariant
      case Contravariant => Covariant
      case Invariant => Invariant

  class TypeMap:
    var localBinders: List[Binder] = Nil
    var variance: Variance = Variance.Covariant

    def withBinder[T](bd: Binder)(op: => T): T =
      localBinders = bd :: localBinders
      try op finally localBinders = localBinders.tail

    def withVariance[T](v: Variance)(op: => T): T =
      val old = variance
      variance = v
      try op finally variance = old

    def apply(tp: Type): Type = mapOver(tp)

    def mapBinder(param: Binder): Binder = param match
      case TermBinder(name, tpe) => TermBinder(name, apply(tpe)).maybeWithPosFrom(param)
      case TypeBinder(name, bound) => TypeBinder(name, apply(bound)).maybeWithPosFrom(param)
      case CaptureBinder(name, bound) => CaptureBinder(name, mapCaptureSet(bound)).maybeWithPosFrom(param)

    def mapCaptureSet(captureSet: CaptureSet): CaptureSet =
      CaptureSet(captureSet.elems.map(mapCaptureRef)).maybeWithPosFrom(captureSet)

    def mapCaptureRef(ref: CaptureRef): CaptureRef = ref

    def mapOver(tp: Type): Type = tp match
      case Type.Capturing(inner, captureSet) =>
        Type.Capturing(apply(inner), mapCaptureSet(captureSet)).like(tp)
      case Type.TermArrow(params, result) => 
        def go(ps: List[TermBinder], bs: List[Binder]): Type =
          ps match
            case Nil => Type.TermArrow(bs.reverse.asInstanceOf, apply(result)).like(tp)
            case p :: ps =>
              val p1 = withVariance(variance.negate):
                mapBinder(p)
              withBinder(p1):
                go(ps, p1 :: bs)
        go(params, Nil)
      case Type.TypeArrow(params, result) =>
        def go(ps: List[TypeBinder | CaptureBinder], bs: List[Binder]): Type =
          ps match
            case Nil => Type.TypeArrow(bs.reverse.asInstanceOf, apply(result)).like(tp)
            case p :: ps =>
              val p1 = withVariance(variance.negate):
                mapBinder(p)
              withBinder(p1):
                go(ps, p1 :: bs)
        go(params, Nil)
      case _ => tp

  class ShiftType(amount: Int) extends TypeMap:
    override def mapCaptureRef(ref: CaptureRef): CaptureRef = ref match
      case CaptureRef.Ref(Term.BinderRef(idx)) if idx >= localBinders.size =>
        CaptureRef.Ref(Term.BinderRef(idx + amount)).maybeWithPosFrom(ref)
      case ref => ref

    override def apply(tp: Type): Type =
      tp match
        case Type.BinderRef(idx) if idx >= localBinders.size =>
          Type.BinderRef(idx + amount).like(tp)
        case _ => mapOver(tp)
      

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
        case TermBinder(name, tpe) => TermBinder(name, tpe.shift(amount)).maybeWithPosFrom(binder)
        case TypeBinder(name, bound) => TypeBinder(name, bound.shift(amount)).maybeWithPosFrom(binder)
        case CaptureBinder(name, bound) => CaptureBinder(name, bound.shift(amount)).maybeWithPosFrom(binder)
