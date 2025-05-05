package cavia
package core
package ast

import scala.collection.mutable.Set
import Syntax.AccessMode

object Expr:
  /** A trait for expressions that have a type */
  trait Typed:
    private var myTpe: Type = Type.NoType()
    private var myCaptured: CaptureSet | Null = null

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

    def cv: CaptureSet =
      assert(myCaptured != null, s"Expression has no captured variables")
      myCaptured

    def setCV(cv: CaptureSet): Unit =
      myCaptured = cv

    def withCV(cv: CaptureSet): this.type =
      setCV(cv)
      this

    def withCVFrom(other: Typed*): this.type =
      val cv = 
        if other.isEmpty then
          CaptureSet.empty
        else
          other.map(_.cv).reduce(_ ++ _)
      val cv1 = CaptureSet(cv.elems.distinct)
      withCV(cv1)

    def withMoreCV(more: CaptureSet): this.type =
      withCV(myCaptured ++ more)

  enum BaseType:
    /** Base types in the type system */
    case StrType
    case IntType
    case UnitType
    case CharType
    case AnyType
    case NothingType
    case BoolType  // will be represented as i32
    /** Primitive types in WebAssembly */
    case I32, I64
    case F32, F64
    /** Array type: `array[T]` */
    case ArrayType  // it is a type constructor
    // /** Type for the capability of boundary/break: `Break[T]` */
    // case BreakType

    def isIntegralType: Boolean = this match
      case I32 | I64 | IntType => true
      case _ => false

    def isFloatingType: Boolean = this match
      case F32 | F64 => true
      case _ => false

  enum CapKind:
    /** A fresh cap, either coming from a fresh parameter or a fresh application result */
    case Fresh(level: Int)
    /** A cap inst that is separate, coming from a normal term parameter */
    case Sep(level: Int)

    /** The level of the cap instance. */
    val level: Int

  enum CaptureRef extends Positioned:
    case Ref(tp: SingletonType)
    case CAP()
    case CapInst(capId: Int, kind: CapKind, fromInst: Option[Int] = None)

  object CaptureRef:
    private var nextCapId: Int = 0

    def makeCapInst(kind: CapKind, fromInst: Option[Int] = None): CaptureRef.CapInst =
      val result: CaptureRef.CapInst = CaptureRef.CapInst(nextCapId, kind, fromInst)
      nextCapId += 1
      result

  case class QualifiedRef(mode: Syntax.AccessMode, core: CaptureRef) extends Positioned

  sealed trait CaptureSet extends Positioned:
    import CaptureSet.*
    def elems: List[QualifiedRef]
    def ++ (other: CaptureSet): CaptureSet =
      (this, other) match
        case (Const(xs), Const(ys)) => Const(xs ++ ys)
        case (cs: Const, univ: UniversalSet) => univ.mergeConst(cs)
        case (univ: UniversalSet, cs: Const) => univ.mergeConst(cs)
        case (univ1: UniversalSet, univ2: UniversalSet) => univ1.merge(univ2)
        case _ => assert(false, s"Cannot concatenate $this and $other")

    /** Qualify the capture set with an access mode */
    def qualify(mode: Syntax.AccessMode): CaptureSet = mode match
      case Syntax.AccessMode.Normal() => this
      case _ => qualifyUnchecked(mode)

    def qualifyUnchecked(mode: Syntax.AccessMode): CaptureSet =
      val elems1 = elems.map(ref => QualifiedRef(mode, ref.core).maybeWithPosFrom(ref))
      Const(elems1)

  object CaptureSet:
    case class Const(elems: List[QualifiedRef]) extends CaptureSet
    case class UniversalSet(existingRefs: List[QualifiedRef]) extends CaptureSet:
      private var mySolved: Boolean = false
      def solved: Boolean = mySolved
      private var absorbed: Set[QualifiedRef] = Set.empty
      def elems: List[QualifiedRef] =
        assert(solved, "getting elems of an unsolved universal set")
        absorbed.toList ++ existingRefs.toList

      def solve(): CaptureSet =
        mySolved = true
        CaptureSet(elems)

      def absorb(ref: QualifiedRef): Unit =
        absorbed += ref

      def merge(other: UniversalSet): UniversalSet =
        absorbed ++= other.absorbed
        val res = new UniversalSet(existingRefs ++ other.existingRefs)
        res.absorbed = absorbed
        res

      def mergeConst(other: Const): UniversalSet =
        val res = new UniversalSet(existingRefs ++ other.elems)
        res.absorbed = absorbed
        res

      def absorbedRefs: List[QualifiedRef] = absorbed.toList

    def apply(elems: List[QualifiedRef]): CaptureSet = Const(elems)
    def empty: CaptureSet = Const(Nil)
    def universal: CaptureSet = Const(List(QualifiedRef(AccessMode.Normal(), CaptureRef.CAP())))

  extension (cset: CaptureSet)
    def isUnsolvedUniversal: Boolean = cset match
      case CaptureSet.Const(_) => false
      case univ: CaptureSet.UniversalSet => !univ.solved

  enum TypeKind:
    case Star  // *
    case Arrow(params: List[Variance], res: TypeKind)  // [+T1, -T2, T3, ...] -> K

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
    case TermBinder(name: String, tpe: Type, isConsume: Boolean)
    case TypeBinder(name: String, bound: Type)
    case CaptureBinder(name: String, bound: CaptureSet)

    val name: String

    def kindStr: String = this match
      case TermBinder(_, _, _) => "term"
      case TypeBinder(_, _) => "type"
      case CaptureBinder(_, _) => "capture"

  import Binder.*

  // Marker trait for singleton (or, path) types
  sealed trait SingletonType

  enum Type extends Positioned, HasKind:
    /** Base types */
    case Base(base: BaseType)
    /** Reference to a bound type variable */
    case BinderRef(idx: Int)
    /** Reference to a struct symbol */
    case SymbolRef(sym: StructSymbol)
    /** A capturing type, S^C */
    case Capturing(inner: Type, isReadOnly: Boolean, captureSet: CaptureSet)
    /** Function type (z: T1) -> T2 */
    case TermArrow(params: List[TermBinder], result: Type)
    /** Type function type [X, cap C, ...] -> T */
    case TypeArrow(params: List[TypeBinder | CaptureBinder], result: Type)
    /** Applied type constructors */
    case AppliedType(constructor: Type, args: List[Type | CaptureSet])
    /** Refined type T with { ... } */
    case RefinedType(base: Type, refinements: List[FieldInfo])
    /** A singleton type for a variable, x.type */
    case Var(ref: VarRef) extends Type, SingletonType
    /** A path singleton type, e.g. p.a.type */
    case Select(base: SingletonType, fieldInfo: FieldInfo) extends Type, SingletonType
    /** <notype>, a placeholder type in the implementation */
    case NoType()
    /** A type variable, created when type inference. */
    case TypeVar(var instance: Type = NoType())

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
      case NoType() => false
      case _ => true

    def derivedCapturing(inner1: Type, isReadOnly1: Boolean, captureSet1: CaptureSet): Type =
      this match
        case Type.Capturing(inner, isReadOnly, captureSet) if (inner eq inner1) && (isReadOnly == isReadOnly1) && (captureSet eq captureSet1) => this
        case _ => Type.Capturing(inner1, isReadOnly1, captureSet1).like(this)

    def derivedTermArrow(params1: List[Binder], result1: Type): Type =
      this match
        case Type.TermArrow(params, result) if (params eq params1) && (result eq result1) => this
        case _ => Type.TermArrow(params1.asInstanceOf[List[TermBinder]], result1).like(this)

    def derivedTypeArrow(params1: List[Binder], result1: Type): Type =
      this match
        case Type.TypeArrow(params, result) if (params eq params1) && (result eq result1) => this
        case _ => Type.TypeArrow(params1.asInstanceOf[List[TypeBinder | CaptureBinder]], result1).like(this)
    
    def derivedAppliedType(constructor1: Type, args1: List[Type | CaptureSet]): Type =
      this match
        case Type.AppliedType(constructor, args) if (constructor eq constructor1) && (args eq args1) => this
        case _ => Type.AppliedType(constructor1, args1).like(this)

    def derivedRefinedType(base1: Type, refinements1: List[FieldInfo]): Type =
      this match
        case Type.RefinedType(base, refinements) if (base eq base1) && (refinements eq refinements1) => this
        case _ => Type.RefinedType(base1, refinements1).like(this)

    def derivedSelect(base1: SingletonType, fieldInfo1: FieldInfo): Type =
      this match
        case Type.Select(base, fieldInfo) if (base eq base1) && (fieldInfo eq fieldInfo1) => this
        case _ => Type.Select(base1, fieldInfo1).like(this)
  
  object Type:
    private var nextId: Int = 0

  // marker trait for primitive ops that are related to arrays
  trait ArrayPrimitiveOp

  /** Language primitive operations */
  enum PrimitiveOp extends Positioned:
    case I32Add
    case I32Mul
    case I32Sub
    case I32Div
    case I32Rem
    case I32Eq
    case I32Neq
    case I32Lt
    case I32Gt
    case I32Lte
    case I32Gte
    case I64Add
    case I64Mul
    case I64Sub
    case I64Div
    case I64Rem
    case I64Eq
    case I64Neq
    case I64Lt
    case I64Gt
    case I64Lte
    case I64Gte
    case I32Println
    case I32Read
    case BoolEq
    case BoolNeq
    case BoolNot
    case BoolAnd
    case BoolOr
    case I32Neg
    case I64Neg
    /** Setting a field of a struct */
    case StructSet
    /** Array ops */
    case ArrayNew extends PrimitiveOp, ArrayPrimitiveOp
    case ArraySet extends PrimitiveOp, ArrayPrimitiveOp
    case ArrayGet extends PrimitiveOp, ArrayPrimitiveOp
    case ArrayLen extends PrimitiveOp, ArrayPrimitiveOp
    /** A primitive in the very virtue of Lean */
    case Sorry
    /** Print a character */
    case PutChar
    /** Convert a value to a pure value in an unsafe way */
    case UnsafeAsPure
    /** Get the elapsed time since the start of the program in milliseconds */
    case PerfCounter
    /** Return a value */
    case Return
    // /** Set up a boundary */
    // case Boundary

    override def toString: String = this match
      case I32Add => "#i32add"
      case I32Mul => "#i32mul"
      case I64Add => "#i64add"
      case I64Mul => "#i64mul"
      case I32Println => "#i32println"
      case I32Read => "#i32read"
      case Sorry => "sorry"
      case I32Sub => "#i32sub"
      case I32Div => "#i32div"
      case I32Rem => "#i32rem"
      case I64Sub => "#i64sub"
      case I64Div => "#i64div"
      case I64Rem => "#i64rem"
      case I32Eq => "#i32eq"
      case I32Neq => "#i32neq"
      case I32Lt => "#i32lt"
      case I32Gt => "#i32gt"
      case I32Lte => "#i32lte"
      case I32Gte => "#i32gte"
      case I64Eq => "#i64eq"
      case I64Neq => "#i64neq"
      case I64Lt => "#i64lt"
      case I64Gt => "#i64gt"
      case I64Lte => "#i64lte"
      case I64Gte => "#i64gte"
      case BoolEq => "#booleq"
      case BoolNeq => "#boolneq"
      case BoolNot => "#boolnot"
      case BoolAnd => "#booland"
      case BoolOr => "#boolor"
      case I32Neg => "#i32neg"
      case I64Neg => "#i64neg"
      case StructSet => "#structset"
      case ArrayNew => "#arraynew"
      case ArraySet => "#arrayset"
      case ArrayGet => "#arrayget"
      case ArrayLen => "#arraylen"
      case PutChar => "#putchar"
      case PerfCounter => "#perfcounter"
      case UnsafeAsPure => "#unsafeAsPure"
      case Return => "return"
      // case Boundary => "#boundary"

  object PrimitiveOp:
    def fromName(name: String): Option[PrimitiveOp] = name match
      case "#i32add" => Some(PrimitiveOp.I32Add)
      case "#i32mul" => Some(PrimitiveOp.I32Mul)
      case "#i64add" => Some(PrimitiveOp.I64Add)
      case "#i64mul" => Some(PrimitiveOp.I64Mul)
      case "#i32println" => Some(PrimitiveOp.I32Println)
      case "#i32read" => Some(PrimitiveOp.I32Read)
      case "#i32sub" => Some(PrimitiveOp.I32Sub)
      case "#i32div" => Some(PrimitiveOp.I32Div)
      case "#i32rem" => Some(PrimitiveOp.I32Rem)
      case "#i64sub" => Some(PrimitiveOp.I64Sub)
      case "#i64div" => Some(PrimitiveOp.I64Div)
      case "#i64rem" => Some(PrimitiveOp.I64Rem)
      case "#i32eq" => Some(PrimitiveOp.I32Eq)
      case "#i32neq" => Some(PrimitiveOp.I32Neq)
      case "#i32lt" => Some(PrimitiveOp.I32Lt)
      case "#i32gt" => Some(PrimitiveOp.I32Gt)
      case "#i32lte" => Some(PrimitiveOp.I32Lte)
      case "#i32gte" => Some(PrimitiveOp.I32Gte)
      case "#i64eq" => Some(PrimitiveOp.I64Eq)
      case "#i64neq" => Some(PrimitiveOp.I64Neq)
      case "#i64lt" => Some(PrimitiveOp.I64Lt)
      case "#i64gt" => Some(PrimitiveOp.I64Gt)
      case "#i64lte" => Some(PrimitiveOp.I64Lte)
      case "#i64gte" => Some(PrimitiveOp.I64Gte)
      case "#booleq" => Some(PrimitiveOp.BoolEq)
      case "#boolneq" => Some(PrimitiveOp.BoolNeq)
      case "#boolnot" => Some(PrimitiveOp.BoolNot)
      case "#booland" => Some(PrimitiveOp.BoolAnd)
      case "#boolor" => Some(PrimitiveOp.BoolOr)
      case "#i32neg" => Some(PrimitiveOp.I32Neg)
      case "#i64neg" => Some(PrimitiveOp.I64Neg)
      case "newArray" => Some(PrimitiveOp.ArrayNew)
      case "sorry" => Some(PrimitiveOp.Sorry)
      case "#putchar" => Some(PrimitiveOp.PutChar)
      case "#unsafeAsPure" => Some(PrimitiveOp.UnsafeAsPure)
      case "#perfcounter" => Some(PrimitiveOp.PerfCounter)
      // case "boundary" => Some(PrimitiveOp.Boundary)
      case _ => None

  sealed trait Closure

  enum Term extends Positioned, Typed, HasMetadata:
    case BinderRef(idx: Int)
    case SymbolRef(sym: DefSymbol)
    case StrLit(value: String)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case CharLit(value: Char)
    case UnitLit()
    case TermLambda(params: List[TermBinder], body: Term, skolemizedBinders: List[TermBinder]) extends Term, Closure
    case TypeLambda(params: List[TypeBinder | CaptureBinder], body: Term) extends Term, Closure
    case Bind(binder: TermBinder, recursive: Boolean, bound: Term, body: Term)
    case PrimOp(op: PrimitiveOp, targs: List[Type], args: List[Term])
    case StructInit(sym: StructSymbol, targs: List[Type | CaptureSet], args: List[Term])
    case Apply(fun: Term, args: List[Term])
    case TypeApply(term: Term, targs: List[Type | CaptureSet])
    case Select(base: Term, fieldInfo: FieldInfo)
    case If(cond: Term, thenBranch: Term, elseBranch: Term)
    case ResolveExtension(sym: ExtensionSymbol, targs: List[Type | CaptureSet], methodName: String)

  /** Reference to a variable, either a binder or a symbol */
  type VarRef = Term.BinderRef | Term.SymbolRef

  sealed trait Symbol extends Positioned:
    val name: String
    val from: Module

  case class DefSymbol(name: String, var tpe: Type, from: Module) extends Symbol
  case class StructSymbol(name: String, var info: StructInfo, from: Module) extends Symbol:
    override def toString(): String = s"StructSymbol($name)"
  case class ExtensionSymbol(name: String, var info: ExtensionInfo, from: Module) extends Symbol:
    override def toString(): String = s"ExtensionSymbol($name)"

  enum Definition extends Positioned:
    case ValDef(sym: DefSymbol, body: Term)
    case StructDef(sym: StructSymbol)
    case ExtensionDef(sym: ExtensionSymbol)

  case class Module(var defns: List[Definition])

  case class FieldInfo(name: String, tpe: Type, mutable: Boolean)
  case class StructInfo(targs: List[TypeBinder | CaptureBinder], variances: List[Variance], fields: List[FieldInfo])
  case class ExtensionInfo(typeParams: List[TypeBinder | CaptureBinder], selfArgType: Type, methods: List[ExtensionMethod])
  case class ExtensionMethod(name: String, tpe: Type, body: Term)

  object Definitions:
    def anyType: Type = Type.Base(BaseType.AnyType).withKind(TypeKind.Star)
    def nothingType: Type = Type.Base(BaseType.NothingType).withKind(TypeKind.Star)
    def strType: Type = Type.Base(BaseType.StrType).withKind(TypeKind.Star)
    def intType: Type = Type.Base(BaseType.IntType).withKind(TypeKind.Star)
    def charType: Type = Type.Base(BaseType.CharType).withKind(TypeKind.Star)
    def i64Type: Type = Type.Base(BaseType.I64).withKind(TypeKind.Star)
    def i32Type: Type = Type.Base(BaseType.I32).withKind(TypeKind.Star)
    def unitType: Type = Type.Base(BaseType.UnitType).withKind(TypeKind.Star)
    def boolType: Type = Type.Base(BaseType.BoolType).withKind(TypeKind.Star)
    def arrayConstructorType: Type =
      val kind = TypeKind.Arrow(List(Variance.Invariant), TypeKind.Star)
      Type.Base(BaseType.ArrayType).withKind(kind)
    def arrayType(elemType: Type): Type =
      Type.AppliedType(arrayConstructorType, List(elemType)).withKind(TypeKind.Star)
    // def breakConstructorType: Type =
    //   Type.Base(BaseType.BreakType).withKind(tycon1Kind)
    // def breakCapabilityType(returnType: Type): Type =
    //   Type.AppliedType(breakConstructorType, List(returnType)).withKind(TypeKind.Star)

    val MemorySymbol = DefSymbol("WASM_MEMORY", arrayType(i32Type), Module(Nil))

  enum Variance:
    case Covariant
    case Contravariant
    case Invariant

    def negate: Variance = this match
      case Covariant => Contravariant
      case Contravariant => Covariant
      case Invariant => Invariant

    def *(other: Variance): Variance = this match
      case Covariant => other
      case Contravariant => other.negate
      case Invariant => this
