package cavia.core.ast

object Wasm:
  case class Module(fields: List[ModuleField])

  enum ValType:
    case I32
    case I64
    case TypedRef(sym: Symbol)
    case AnyRef

    def show: String = this match
      case ValType.I32 => "i32"
      case ValType.I64 => "i64"
      case ValType.TypedRef(sym) => s"(ref ${sym.show})"
      case ValType.AnyRef => "anyref"

  enum Instruction:
    case I32Const(value: Int)
    case I32Add
    case I32Mul
    case I64Const(value: Int)
    case I64Add
    case I64Mul
    case LocalSet(sym: Symbol)
    case LocalGet(sym: Symbol)
    case RefCast(typeSym: Symbol)
    case RefFunc(funcSym: Symbol)
    case StructGet(sym: Symbol, fieldSym: Symbol)
    case StructNew(typeSym: Symbol)
    case CallRef(typeSym: Symbol)

    def show: String = this match
      case I32Const(value) => s"i32.const $value"
      case I32Add => "i32.add"
      case I32Mul => "i32.mul"
      case I64Const(value) => s"i64.const $value"
      case I64Add => "i64.add"
      case I64Mul => "i64.mul"
      case LocalSet(sym) => s"local.set ${sym.show}"
      case LocalGet(sym) => s"local.get ${sym.show}"
      case RefCast(typeSym) => s"ref.cast (ref ${typeSym.show})"
      case RefFunc(funcSym) => s"ref.func ${funcSym.show}"
      case StructGet(sym, fieldSym) => s"struct.get ${sym.show} ${fieldSym.show}"
      case StructNew(typeSym) => s"struct.new ${typeSym.show}"
      case CallRef(typeSym) => s"call_ref ${typeSym.show}"

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
  object NoSymbol extends Symbol:
    def show: String = assert(false, "No symbol")

  object Symbol:
    private var nextId = 0
    def fresh(name: String): Symbol =
      val id = nextId
      nextId += 1
      UniqSymbol(name, id)

    val Function = fresh("__func")

  sealed trait CompositeType:
    def show: String
  case class FuncType(paramTypes: List[ValType], resultType: ValType) extends CompositeType:
    def show: String =
      val paramStrs = paramTypes.map(p => s"(param ${p.show})")
      val resultStr = s"(result ${resultType.show})"
      s"(func ${paramStrs.mkString(" ")} ${resultStr})"
  case class StructType(fields: List[(Symbol, ValType)], subClassOf: Option[Symbol]) extends CompositeType:
    def show: String = 
      val fieldStrs = fields.map: (sym, tpe) =>
        s"(field ${sym.show} ${tpe.show})"
      val subclassStr = subClassOf match
        case None => ""
        case Some(sym) => s"${sym.show} "
      s"(sub ${subclassStr}(struct ${fieldStrs.mkString(" ")}))"

  sealed trait ModuleField
  case class Func(ident: Symbol, params: List[(Symbol, ValType)], result: ValType, locals: List[(Symbol, ValType)], body: List[Instruction]) extends ModuleField:
    def tpe: FuncType = FuncType(params.map(_._2), result)
  case class Export(externalName: String, kind: ExportKind, ident: Symbol) extends ModuleField
  case class TypeDef(ident: Symbol, tpe: CompositeType) extends ModuleField
  case class ElemDeclare(kind: ExportKind, sym: Symbol) extends ModuleField
