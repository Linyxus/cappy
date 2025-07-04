package cavia
package core
package ast

import scala.collection.mutable, mutable.ArrayBuffer
import tokenizing.*
import expr.Expr

object Syntax:
  enum AccessMode extends Positioned:
    case Normal()
    case ReadOnly()
    case Consume()

  case class TermParam(name: String, tpe: Option[Type], isConsume: Boolean) extends Positioned
  case class TypeParam(name: String, bound: Option[Type]) extends Positioned
  case class CaptureParam(name: String, bound: Option[CaptureSet]) extends Positioned
  case class CaptureRef(name: String, mode: AccessMode) extends Positioned
  case class CaptureSet(elems: List[CaptureRef]) extends Positioned

  enum InfixOp extends Positioned:
    case Plus, Minus, Mul, Div, Mod, Concat
    case Eq, Neq, Lt, Gt, Lte, Gte
    case And, Or

    override def toString(): String = this match
      case Plus => "+"
      case Minus => "-"
      case Mul => "*"
      case Div => "/"
      case Mod => "%"
      case Concat => "++"
      case Eq => "=="
      case Neq => "!="
      case Lt => "<"
      case Gt => ">"
      case Lte => "<="
      case Gte => ">="
      case And => "&&"
      case Or => "||"

  enum PrefixOp extends Positioned:
    case Neg, Not
    //case Return

    override def toString(): String = this match
      case Neg => "-"
      case Not => "!"

  enum Pattern extends Positioned:
    case Wildcard()
    case Bind(name: String, pat: Pattern)
    case EnumVariant(constructor: String, fields: List[Pattern])
  case class MatchCase(pat: Pattern, body: Term) extends Positioned

  enum Term extends Positioned:
    case Ident(name: String)
    case Select(base: Term, field: String)
    case StrLit(value: String)
    case IntLit(value: Int)
    case FloatLit(value: Float)
    case BoolLit(value: Boolean)
    case CharLit(value: Char)
    case UnitLit()
    case Lambda(params: List[TermParam], body: Term)
    case TypeLambda(params: List[TypeParam | CaptureParam], body: Term)
    case Apply(fun: Term, args: List[Term])
    case TypeApply(term: Term, targs: List[Type | CaptureSet])
    case Block(stmts: List[Definition | Term])
    case Assign(lhs: Term, rhs: Term)
    case Infix(op: InfixOp, lhs: Term, rhs: Term)
    case Prefix(op: PrefixOp, term: Term)
    case If(cond: Term, thenBranch: Term, elseBranch: Option[Term])
    case Match(scrutinee: Term, cases: List[MatchCase])
  import Term.*

  case class TypeParamList(params: List[TypeParam | CaptureParam]) extends Positioned
  case class TermParamList(params: List[TermParam]) extends Positioned

  case class FieldDef(name: String, isVar: Boolean, tpe: Type) extends Positioned

  case class EnumVariantDef(name: String, fields: List[FieldDef]) extends Positioned

  /** A type parameter for a type constructor.
   * The main difference with type parameters of lambdas is that
   * they can be annotated with variance.
   */
  enum ConstructorTypeParam extends Positioned:
    case Typ(param: TypeParam, variance: Int)
    case Cap(param: CaptureParam, variance: Int)

    def toTypeParam: TypeParam | CaptureParam = this match
      case Typ(param, _) => param
      case Cap(param, _) => param

    val variance: Int

  enum ModuleName:
    case Root()
    case Qualified(prefix: ModuleName, name: String)

  case class Module(name: ModuleName, defs: List[Definition], var sourceFile: io.SourceFile) extends Positioned

  sealed trait Annotation extends Positioned
  case class TermAnnotation(term: Term) extends Annotation

  enum Definition extends Positioned:
    /** Annotations for this definition. */
    val annots: mutable.ArrayBuffer[Annotation] = ArrayBuffer.empty

    /** A value definition, like val x: T = ... */
    case ValDef(name: String, tpe: Option[Type], expr: Term)
    /** A function definition, like def f[T](x: T): U = ... */
    case DefDef(name: String, captureSet: Option[CaptureSet], paramss: List[TypeParamList | TermParamList], resultType: Option[Type], expr: Term)
    /** A struct definition, like struct P(x: i32, y: i32) */
    case StructDef(name: String, targs: List[ConstructorTypeParam], fields: List[FieldDef])
    /** An extension definition */
    case ExtensionDef(name: String, typeArgs: List[TypeParam | CaptureParam], selfArg: TermParam, methods: List[DefDef])
    /** A type definition, like type Arr[X] = array[X] */
    case TypeDef(name: String, targs: List[ConstructorTypeParam], body: Type)
    /** An enum definition, like
     * ```
     * enum Option[T]:
     *   case Some(value: T)
     *   case None()
     * ```
     */
    case EnumDef(name: String, targs: List[ConstructorTypeParam], variants: List[EnumVariantDef])

    val name: String
  import Definition.*

  type ValueDef = ValDef | DefDef

  enum Type extends Positioned:
    case Ident(name: String)
    case Arrow(params: List[TermParam], result: Type)
    case TypeArrow(params: List[TypeParam | CaptureParam], result: Type)
    //case CaptureArrow(params: List[CaptureParam], result: Type)
    case Capturing(inner: Type, isReadOnly: Boolean, captureSet: CaptureSet)
    case AppliedType(tycon: Type, args: List[Type | CaptureSet])
    case Boxed(core: Type)
    case Splice(tp: Expr.Type)
  import Type.*
