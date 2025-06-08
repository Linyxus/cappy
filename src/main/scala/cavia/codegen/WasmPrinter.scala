package cavia
package codegen

import core.ast.Wasm.*
import reporting.IndentedPrinter

class WasmPrinter extends IndentedPrinter:
  def printModule(mod: Module): Unit = 
    print("(module")
    newline()
    indented:
      for field <- mod.fields do
        printField(field)
        newline()
    print(")")

  def printField(field: ModuleField): Unit = field match
    case func: Func => printFunc(func)
    case exp: Export => printExport(exp)
    case t: TypeDef => printTypeDef(t)
    case elem: ElemDeclare => printElemDeclare(elem)
    case i: ImportFunc => printImportFunc(i)
    case g: Global => printGlobal(g)
    case s: Start => printStart(s)
    case m: Memory => printMemory(m)
    case t: Table => printTable(t)

  def printInstruction(instruction: Instruction): Unit =
    instruction.showIfSimple match
      case Some(str) => print(str)
      case None =>
        instruction match
          case Instruction.If(resultType, thenBranch, elseBranch) =>
            print(s"(if (result ${resultType.show})")
            newline()
            indented:
              print("(then")
              newline()
              indented:
                thenBranch.foreach: instruction =>
                  printInstruction(instruction)
                  newline()
              newline()
              print(")")
            newline()
            indented:
              print("(else")
              newline()
              indented:
                elseBranch.foreach: instruction =>
                  printInstruction(instruction)
                  newline()
              newline()
              print(")")
            newline()
            print(")")
          case _ => assert(false, s"Unknown instruction: $instruction")

  def printFunc(func: Func): Unit =
    print(s"(func ${func.ident.show} ")
    func.params.foreach: (paramId, paramType) =>
      print(s"(param ${paramId.show} ${paramType.show}) ")
    func.result match
      case None =>
      case Some(resTpe) =>
        print(s"(result ${resTpe.show})")
    newline()
    indented:
      func.locals.foreach: (localId, localType) =>
        print(s"(local ${localId.show} ${localType.show})")
        newline()
      func.body.foreach: instruction =>
        printInstruction(instruction)
        newline()
    print(")")

  def printExport(exp: Export): Unit =
    print(s"(export \"${exp.externalName}\" (${exp.kind.show} ${exp.ident.show}))")

  def printTypeDef(t: TypeDef): Unit =
    print(s"(type ${t.ident.show} ${t.tpe.show})")

  def printMemory(m: Memory): Unit =
    print(s"(memory ${m.ident.show} ${m.size})")

  def printElemDeclare(e: ElemDeclare): Unit =
    print(s"(elem declare ${e.kind.show} ${e.sym.show})")

  def printImportFunc(i: ImportFunc): Unit =
    val paramStr = i.funcType.paramTypes.map(p => s"(param ${p.show})").mkString(" ")
    val resultStr = i.funcType.resultType match
      case None => ""
      case Some(resultType) => s" (result ${resultType.show})"
    print(s"(import \"${i.moduleName}\" \"${i.funcName}\" (func ${i.ident.show} ${paramStr}${resultStr}))")

  def printGlobal(g: Global): Unit =
    val typText = g.tpe.show
    val typeText = if g.mutable then s"(mut ${typText})" else typText
    print(s"(global ${g.ident.show} ${typeText} (${g.init.showIfSimple.get}))")

  def printStart(s: Start): Unit =
    print(s"(start ${s.funcSym.show})")

  def printTable(t: Table): Unit =
    print(s"(table ${t.ident.show} ${t.size} ${t.elemType.show})")

extension (mod: Module)
  def show: String =
    val printer = WasmPrinter()
    printer.printModule(mod)
    printer.result()
