package cavia.core.ast

object Wasm:
  case class Module(fields: List[ModuleField])

  enum ValType:
    case I32
    case I64
    case TypedRef(sym: Symbol, nullable: Boolean = false)
    case AnyRef

    def show: String = this match
      case ValType.I32 => "i32"
      case ValType.I64 => "i64"
      case ValType.TypedRef(sym, nullable) => 
        val nullText = if nullable then "null " else ""
        s"(ref $nullText${sym.show})"
      case ValType.AnyRef => "anyref"

  enum Instruction:
    case I32Const(value: Int)
    case I32Add
    case I32Sub
    case I32Mul
    case I32Div
    case I32Rem
    case I32Gte
    case I32Lte
    case I32Gt
    case I32Lt
    case I32Eq
    case I32Ne
    case I32Eqz
    case I64Const(value: Int)
    case I64Add
    case I64Sub
    case I64Mul
    case I64Div
    case I64Rem
    case I64Gte
    case I64Lte
    case I64Gt
    case I64Lt
    case I64Eq
    case I64Ne
    case I64Eqz
    case LocalSet(sym: Symbol)
    case LocalGet(sym: Symbol)
    case GlobalSet(sym: Symbol)
    case GlobalGet(sym: Symbol)
    case RefCast(typeSym: Symbol)
    case RefFunc(funcSym: Symbol)
    case RefNull(typeSym: Symbol)
    case RefNullAny
    case StructGet(sym: Symbol, fieldSym: Symbol)
    case StructSet(sym: Symbol, fieldSym: Symbol)
    case StructNew(typeSym: Symbol)
    case CallRef(typeSym: Symbol)
    case Call(funcSym: Symbol)
    case If(resultType: ValType, thenBranch: List[Instruction], elseBranch: List[Instruction])
    case ArrayNew(typeSym: Symbol)
    case ArraySet(typeSym: Symbol)
    case ArrayGet(typeSym: Symbol)
    case ArrayLen
    case I32Load(memorySym: Symbol)
    case I32Store(memorySym: Symbol)
    case MemorySize(memorySym: Symbol)

    def showIfSimple: Option[String] = this match
      case I32Const(value) => Some(s"i32.const $value")
      case I32Add => Some("i32.add")
      case I32Sub => Some("i32.sub")
      case I32Mul => Some("i32.mul")
      case I32Div => Some("i32.div_s")
      case I32Rem => Some("i32.rem_s")
      case I32Gte => Some("i32.ge_s")
      case I32Lte => Some("i32.le_s")
      case I32Gt => Some("i32.gt_s")
      case I32Lt => Some("i32.lt_s")
      case I32Eq => Some("i32.eq")
      case I32Ne => Some("i32.ne")
      case I32Eqz => Some("i32.eqz")
      case I64Const(value) => Some(s"i64.const $value")
      case I64Add => Some("i64.add")
      case I64Sub => Some("i64.sub")
      case I64Mul => Some("i64.mul")
      case I64Div => Some("i64.div_s")
      case I64Rem => Some("i64.rem_s")
      case I64Gte => Some("i64.ge_s")
      case I64Lte => Some("i64.le_s")
      case I64Gt => Some("i64.gt_s")
      case I64Lt => Some("i64.lt_s")
      case I64Eq => Some("i64.eq")
      case I64Ne => Some("i64.ne")
      case I64Eqz => Some("i64.eqz")
      case LocalSet(sym) => Some(s"local.set ${sym.show}")
      case LocalGet(sym) => Some(s"local.get ${sym.show}")
      case GlobalSet(sym) => Some(s"global.set ${sym.show}")
      case GlobalGet(sym) => Some(s"global.get ${sym.show}")
      case RefCast(typeSym) => Some(s"ref.cast (ref ${typeSym.show})")
      case RefFunc(funcSym) => Some(s"ref.func ${funcSym.show}")
      case StructGet(sym, fieldSym) => Some(s"struct.get ${sym.show} ${fieldSym.show}")
      case StructSet(sym, fieldSym) => Some(s"struct.set ${sym.show} ${fieldSym.show}")
      case StructNew(typeSym) => Some(s"struct.new ${typeSym.show}")
      case CallRef(typeSym) => Some(s"call_ref ${typeSym.show}")
      case Call(funcSym) => Some(s"call ${funcSym.show}")
      case RefNull(typeSym) => Some(s"ref.null ${typeSym.show}")
      case RefNullAny => Some("ref.null any")
      case ArrayNew(typeSym) => Some(s"array.new ${typeSym.show}")
      case ArraySet(typeSym) => Some(s"array.set ${typeSym.show}")
      case ArrayGet(typeSym) => Some(s"array.get ${typeSym.show}")
      case ArrayLen => Some(s"array.len")
      case I32Load(memorySym) => 
        Some(s"i32.load ${memorySym.show}")
      case I32Store(memorySym) => 
        Some(s"i32.store ${memorySym.show}")
      case MemorySize(memorySym) =>
        Some(s"memory.size ${memorySym.show}")
      case _ => None

  enum ExportKind:
    case Func
    case Table
    case Memory

    def show: String = this match
      case ExportKind.Func => "func"
      case ExportKind.Table => "table"
      case ExportKind.Memory => "memory"

  sealed trait Symbol:
    def show: String
  case class UniqSymbol(val name: String, val uniqId: Int) extends Symbol:
    def show: String = s"$$${name}@${uniqId}"

    override def hashCode(): Int = name.hashCode() + uniqId + 1

  object NoSymbol extends Symbol:
    def show: String = assert(false, "No symbol")
    override def hashCode(): Int = 0

  object Symbol:
    private var nextId = 0
    def fresh(name: String): Symbol =
      val id = nextId
      nextId += 1
      UniqSymbol(name, id)

    val Function = fresh("__func")
    val I32Println = fresh("__i32println")
    val I32Read = fresh("__i32read")
    val Memory = fresh("__memory")

  val I32PrintlnType = FuncType(List(ValType.I32), None)
  val I32ReadType = FuncType(List(), Some(ValType.I32))

  case class FieldType(sym: Symbol, tpe: ValType, mutable: Boolean)

  sealed trait CompositeType:
    def show: String

  case class FuncType(paramTypes: List[ValType], resultType: Option[ValType]) extends CompositeType:
    def show: String =
      val paramStrs = paramTypes.map(p => s"(param ${p.show})")
      val resultStr = 
        resultType match
          case None => ""
          case Some(resultType) => s"(result ${resultType.show})"
      s"(func ${paramStrs.mkString(" ")} ${resultStr})"
  case class StructType(fields: List[FieldType], subClassOf: Option[Symbol]) extends CompositeType:
    def show: String = 
      val fieldStrs = fields.map: 
        case FieldType(sym, tpe, mutable) =>
          val typeStr = if mutable then s"(mut ${tpe.show})" else tpe.show
          s"(field ${sym.show} $typeStr)"
      val subclassStr = subClassOf match
        case None => ""
        case Some(sym) => s"${sym.show} "
      s"(sub ${subclassStr}(struct ${fieldStrs.mkString(" ")}))"
  case class ArrayType(elemType: ValType, mutable: Boolean) extends CompositeType:
    def show: String = 
      val typeStr = if mutable then s"(mut ${elemType.show})" else elemType.show
      s"(array $typeStr)"

  sealed trait ModuleField
  case class Func(ident: Symbol, params: List[(Symbol, ValType)], result: Option[ValType], locals: List[(Symbol, ValType)], body: List[Instruction]) extends ModuleField:
    def tpe: FuncType = FuncType(params.map(_._2), result)
  case class Export(externalName: String, kind: ExportKind, ident: Symbol) extends ModuleField
  case class TypeDef(ident: Symbol, tpe: CompositeType) extends ModuleField
  case class ElemDeclare(kind: ExportKind, sym: Symbol) extends ModuleField
  case class ImportFunc(moduleName: String, funcName: String, ident: Symbol, funcType: FuncType) extends ModuleField
  case class Global(ident: Symbol, tpe: ValType, mutable: Boolean, init: Instruction) extends ModuleField
  case class Start(funcSym: Symbol) extends ModuleField
  case class Memory(ident: Symbol, size: Int) extends ModuleField