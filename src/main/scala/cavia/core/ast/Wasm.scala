package cavia.core.ast

object Wasm:
  case class Module(fields: List[ModuleField])

  enum ValType:
    case I32
    case I64
    case FuncRef
    case StructRef(sym: Symbol)

    def show: String = this match
      case ValType.I32 => "i32"
      case ValType.I64 => "i64"
      case ValType.FuncRef => "funcref"
      case ValType.StructRef(sym) => s"(ref ${sym.show})"

  enum Instruction:
    case I32Const(value: Int)
    case I32Add
    case I32Mul
    case I64Const(value: Int)
    case I64Add
    case I64Mul
    case LocalSet(sym: Symbol)
    case LocalGet(sym: Symbol)

    def show: String = this match
      case I32Const(value) => s"i32.const $value"
      case I32Add => "i32.add"
      case I32Mul => "i32.mul"
      case I64Const(value) => s"i64.const $value"
      case I64Add => "i64.add"
      case I64Mul => "i64.mul"
      case LocalSet(sym) => s"local.set ${sym.show}"
      case LocalGet(sym) => s"local.get ${sym.show}"

  enum ExportKind:
    case Func
    case Table
    case Memory

    def show: String = this match
      case ExportKind.Func => "func"
      case ExportKind.Table => "table"
      case ExportKind.Memory => "memory"

  case class Symbol(val name: String, val uniqId: Int):
    def show: String = s"$$${name}@${uniqId}"

  object Symbol:
    private var nextId = 0
    def fresh(name: String): Symbol =
      val id = nextId
      nextId += 1
      Symbol(name, id)

  case class FuncType(paramTypes: List[ValType], resultType: ValType)

  sealed trait ModuleField
  case class Func(ident: Symbol, params: List[(Symbol, ValType)], result: ValType, locals: List[(Symbol, ValType)], body: List[Instruction]) extends ModuleField:
    def tpe: FuncType = FuncType(params.map(_._2), result)
  case class Export(externalName: String, kind: ExportKind, ident: Symbol) extends ModuleField
