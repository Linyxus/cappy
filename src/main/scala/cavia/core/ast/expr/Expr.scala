package cavia
package core
package ast
package expr

import scala.collection.mutable.Set
import Syntax.AccessMode
import typechecking.TypeVarInfo

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
    /** Type for regions */
    case RegionType
    /** Type for references allocated on regions. It is a type constructor `* -> *`. */
    case RegionRefType

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
        CaptureSet(absorbed.toList)

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
    case SymbolRef(sym: TypeSymbol)
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
    case TypeVar(info: TypeVarInfo)
    /** A boxed type, box T^{...} */
    case Boxed(core: Type)

    def like(other: Type): this.type =
      assert(other.hasKind, s"Type $other (id=${other.id}) does not have a kind when calling like")
      if other.hasPos then
        this.withPosFrom(other)
      this.withKind(other.kind)

    val id: Int =
      Type.nextId += 1
      //assert(Type.nextId != 1237, "Gotcha!")  // for debugging
      Type.nextId

    def exists: Boolean = this match
      case NoType() => false
      case _ => true

    def derivedBoxed(core1: Type): Type =
      this match
        case Type.Boxed(core) if (core eq core1) => this
        case _ => Type.Boxed(core1).like(this)

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
  sealed trait PrimitiveOp extends Positioned

  enum BasicPrimOpKind:
    case Add
    case Mul
    case Sub
    case Div
    case Rem
    case Eq
    case Neq
    case Lt
    case Gt
    case Lte
    case Gte
    case Neg
    case And
    case Or
    case Not

  sealed trait BasicPrimOp(val opKind: BasicPrimOpKind, val operandTypes: List[BaseType], val resultType: BaseType) extends PrimitiveOp
  case class BinaryPrimOp(k: BasicPrimOpKind, val operandType: BaseType) extends BasicPrimOp(k, List(operandType, operandType), operandType):
    override def toString(): String = s"#$operandType.$opKind"
  case class UnaryPrimOp(k: BasicPrimOpKind, val operandType: BaseType) extends BasicPrimOp(k, List(operandType), operandType):
    override def toString(): String = s"#$operandType.$opKind"
  case class ComparePrimOp(k: BasicPrimOpKind, val operandType: BaseType) extends BasicPrimOp(k, List(operandType, operandType), BaseType.BoolType):
    override def toString(): String = s"#$operandType.$opKind"

  def isBoolAnd(op: PrimitiveOp): Boolean = op match
    case BinaryPrimOp(BasicPrimOpKind.And, _) => true
    case _ => false
  def isBoolOr(op: PrimitiveOp): Boolean = op match
    case BinaryPrimOp(BasicPrimOpKind.Or, _) => true
    case _ => false
  def isBoolNot(op: PrimitiveOp): Boolean = op match
    case UnaryPrimOp(BasicPrimOpKind.Not, _) => true
    case _ => false

  object BasicPrimOpFamily:
    import BasicPrimOpKind.*
    val numericTypes: Set[BaseType] = Set(BaseType.I32, BaseType.I64, BaseType.CharType)
    val logicalTypes: Set[BaseType] = Set(BaseType.BoolType)
    def resolve(opKind: BasicPrimOpKind, inputArgType: BaseType): Option[BasicPrimOp] = opKind match
      case Add | Mul | Sub | Div | Rem if numericTypes.contains(inputArgType) =>
        Some(new BinaryPrimOp(opKind, inputArgType))
      case Eq | Neq | Lt | Gt | Lte | Gte if numericTypes.contains(inputArgType) =>
        Some(new ComparePrimOp(opKind, inputArgType))
      case Neg if numericTypes.contains(inputArgType) =>
        Some(new UnaryPrimOp(opKind, inputArgType))
      case Eq | Neq if logicalTypes.contains(inputArgType) =>
        Some(new ComparePrimOp(opKind, inputArgType))
      case And | Or if logicalTypes.contains(inputArgType) =>
        Some(new BinaryPrimOp(opKind, inputArgType))
      case Not if logicalTypes.contains(inputArgType) =>
        Some(new UnaryPrimOp(opKind, inputArgType))
      case _ => None
  
  // I/O operations
  case class I32Println() extends PrimitiveOp:
    override def toString: String = "#i32println"
  case class I32Read() extends PrimitiveOp:
    override def toString: String = "#i32read"
  
  /** Setting a field of a struct */
  case class StructSet() extends PrimitiveOp:
    override def toString: String = "#structset"
  
  /** Array ops */
  case class ArrayNew() extends PrimitiveOp with ArrayPrimitiveOp:
    override def toString: String = "#arraynew"
  case class ArraySet() extends PrimitiveOp with ArrayPrimitiveOp:
    override def toString: String = "#arrayset"
  case class ArrayGet() extends PrimitiveOp with ArrayPrimitiveOp:
    override def toString: String = "#arrayget"
  case class ArrayLen() extends PrimitiveOp with ArrayPrimitiveOp:
    override def toString: String = "#arraylen"
  
  /** A primitive in the very virtue of Lean */
  case class Sorry() extends PrimitiveOp:
    override def toString: String = "sorry"
  
  /** Print a character */
  case class PutChar() extends PrimitiveOp:
    override def toString: String = "#putchar"
  
  /** Convert a value to a pure value in an unsafe way */
  case class UnsafeAsPure() extends PrimitiveOp:
    override def toString: String = "#unsafeAsPure"
  
  /** Get the elapsed time since the start of the program in milliseconds */
  case class PerfCounter() extends PrimitiveOp:
    override def toString: String = "#perfcounter"
  
  // /** Return a value */
  // case class Return() extends PrimitiveOp:
  //   override def toString: String = "return"
  // /** Set up a boundary */
  // case class Boundary() extends PrimitiveOp:
  //   override def toString: String = "#boundary"
  
  /** Primitives for boxing and unboxing in capture tracking */
  case class Box() extends PrimitiveOp:
    override def toString: String = "#box"
  case class Unbox() extends PrimitiveOp:
    override def toString: String = "#unbox"
  
  /** Open an arena */
  case class Arena() extends PrimitiveOp:
    override def toString: String = "arena"
  
  /** Allocate a struct */
  case class RegionAlloc() extends PrimitiveOp:
    override def toString: String = "#regionalloc"

  object PrimitiveOp:
    def fromName(name: String): Option[PrimitiveOp] = name match
      case "#i32add" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Add, BaseType.I32)
      case "#i32mul" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Mul, BaseType.I32)
      case "#i64add" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Add, BaseType.I64)
      case "#i64mul" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Mul, BaseType.I64)
      case "#i32println" => Some(I32Println())
      case "#i32read" => Some(I32Read())
      case "#i32sub" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Sub, BaseType.I32)
      case "#i32div" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Div, BaseType.I32)
      case "#i32rem" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Rem, BaseType.I32)
      case "#i64sub" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Sub, BaseType.I64)
      case "#i64div" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Div, BaseType.I64)
      case "#i64rem" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Rem, BaseType.I64)
      case "#i32eq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Eq, BaseType.I32)
      case "#i32neq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Neq, BaseType.I32)
      case "#i32lt" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Lt, BaseType.I32)
      case "#i32gt" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Gt, BaseType.I32)
      case "#i32lte" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Lte, BaseType.I32)
      case "#i32gte" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Gte, BaseType.I32)
      case "#i64eq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Eq, BaseType.I64)
      case "#i64neq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Neq, BaseType.I64)
      case "#i64lt" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Lt, BaseType.I64)
      case "#i64gt" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Gt, BaseType.I64)
      case "#i64lte" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Lte, BaseType.I64)
      case "#i64gte" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Gte, BaseType.I64)
      case "#booleq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Eq, BaseType.BoolType)
      case "#boolneq" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Neq, BaseType.BoolType)
      case "#boolnot" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Not, BaseType.BoolType)
      case "#booland" => BasicPrimOpFamily.resolve(BasicPrimOpKind.And, BaseType.BoolType)
      case "#boolor" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Or, BaseType.BoolType)
      case "#i32neg" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Neg, BaseType.I32)
      case "#i64neg" => BasicPrimOpFamily.resolve(BasicPrimOpKind.Neg, BaseType.I64)
      case "#box" => Some(Box())
      case "#unbox" => Some(Unbox())
      case "newArray" => Some(ArrayNew())
      case "sorry" => Some(Sorry())
      case "#putchar" => Some(PutChar())
      case "#unsafeAsPure" => Some(UnsafeAsPure())
      case "#perfcounter" => Some(PerfCounter())
      // case "boundary" => Some(Boundary())
      case "arena" => Some(Arena())
      case _ => None

  /** Marker trait for all closures: term/type lambdas */
  sealed trait Closure

  enum Pattern extends Positioned, Typed:
    case Wildcard()
    case Bind(binder: TermBinder, pat: Pattern)
    case EnumVariant(constructor: StructSymbol, typeArgs: List[Type | CaptureSet], enumSym: Option[EnumSymbol], fields: List[Pattern])

  case class MatchCase(pat: Pattern, body: Term) extends Positioned, Typed:
    def derivedMatchCase(pat1: Pattern, body1: Term): MatchCase =
      if (pat1 eq pat) && (body1 eq body) then
        this
      else
        MatchCase(pat1, body1).withPosFrom(this).withTpe(this.tpe)

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
    case Match(scrutinee: Term, cases: List[MatchCase])
    case ResolveExtension(sym: ExtensionSymbol, targs: List[Type | CaptureSet], methodName: String)

  /** Reference to a variable, either a binder or a symbol */
  type VarRef = Term.BinderRef | Term.SymbolRef

  object Symbol:
    private var nextSymbolId: Int = 0
  sealed trait Symbol extends Positioned:
    val id: Int =
      Symbol.nextSymbolId += 1
      Symbol.nextSymbolId
    val name: String
    val from: Module
    override def hashCode(): Int = id
  sealed trait TypeSymbol extends Symbol

  case class DefSymbol(name: String, var tpe: Type, from: Module) extends Symbol:
    override def toString(): String = s"DefSymbol($name, $tpe)"
  case class StructSymbol(name: String, var info: StructInfo, from: Module) extends TypeSymbol:
    override def toString(): String = s"StructSymbol($name)"
  case class ExtensionSymbol(name: String, var info: ExtensionInfo, from: Module) extends TypeSymbol:
    override def toString(): String = s"ExtensionSymbol($name)"
  case class TypeDefSymbol(name: String, var info: TypeDefInfo, from: Module) extends TypeSymbol:
    override def toString(): String = s"TypeDefSymbol($name)"
  case class EnumSymbol(name: String, var info: EnumInfo, from: Module) extends TypeSymbol:
    override def toString(): String = s"EnumSymbol($name)"

  enum Definition extends Positioned:
    case ValDef(sym: DefSymbol, body: Term)
    case StructDef(sym: StructSymbol)
    case ExtensionDef(sym: ExtensionSymbol)
    case TypeDef(sym: TypeDefSymbol)
    case EnumDef(sym: EnumSymbol)

    val sym: Symbol

  case class Module(name: String, var defns: List[Definition], parent: Module | Null = null)

  /** Denotation of struct types. */
  case class FieldInfo(name: String, tpe: Type, mutable: Boolean)
  case class StructInfo(targs: List[TypeBinder | CaptureBinder], variances: List[Variance], fields: List[FieldInfo], enumSymbol: Option[EnumSymbol] = None)

  /** Denotation of enum types. */
  case class EnumInfo(targs: List[TypeBinder | CaptureBinder], variances: List[Variance], variants: List[StructSymbol])

  /** Denotation of extensions. */
  case class ExtensionMethod(name: String, tpe: Type, body: Term)
  case class ExtensionInfo(typeParams: List[TypeBinder | CaptureBinder], selfArgType: Type, methods: List[ExtensionMethod])

  /** Denotation of type definitions. */
  case class TypeDefInfo(typeParams: List[TypeBinder | CaptureBinder], variances: List[Variance], body: Type)

  val defns: Definitions.type = Definitions
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
    def regionType: Type =
      Type.Base(BaseType.RegionType).withKind(TypeKind.Star)
    def regionRefConstructorType: Type =
      val kind = TypeKind.Arrow(List(Variance.Covariant), TypeKind.Star)
      Type.Base(BaseType.RegionRefType).withKind(kind)
    def regionRefType(elemType: Type): Type =
      Type.AppliedType(regionRefConstructorType, List(elemType)).withKind(TypeKind.Star)
    // def breakConstructorType: Type =
    //   Type.Base(BaseType.BreakType).withKind(tycon1Kind)
    // def breakCapabilityType(returnType: Type): Type =
    //   Type.AppliedType(breakConstructorType, List(returnType)).withKind(TypeKind.Star)
    def structConstructorType(sym: StructSymbol): Type =
      val variances = sym.info.variances
      val kind = 
        if variances.isEmpty then
          TypeKind.Star
        else
          TypeKind.Arrow(variances, TypeKind.Star)
      Type.SymbolRef(sym).withKind(kind)

    def enumConstructorType(sym: EnumSymbol): Type =
      val variances = sym.info.variances
      val kind =
        if variances.isEmpty then
          TypeKind.Star
        else
          TypeKind.Arrow(variances, TypeKind.Star)
      Type.SymbolRef(sym).withKind(kind)

    val RootModule = Module("root", defns = Nil)
    val MemorySymbol = DefSymbol("WASM_MEMORY", arrayType(i32Type), RootModule)

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
