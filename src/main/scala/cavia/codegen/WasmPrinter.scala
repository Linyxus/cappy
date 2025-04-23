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
        print(s"${instruction.show}")
        newline()
    print(")")

  def printExport(exp: Export): Unit =
    print(s"(export \"${exp.externalName}\" (${exp.kind.show} ${exp.ident.show}))")

  def printTypeDef(t: TypeDef): Unit =
    print(s"(type ${t.ident.show} ${t.tpe.show})")

  def printElemDeclare(e: ElemDeclare): Unit =
    print(s"(elem declare ${e.kind.show} ${e.sym.show})")

  def printImportFunc(i: ImportFunc): Unit =
    val paramStr = i.funcType.paramTypes.map(p => s"(param ${p.show})").mkString(" ")
    val resultStr = i.funcType.resultType match
      case None => ""
      case Some(resultType) => s" (result ${resultType.show})"
    print(s"(import \"${i.moduleName}\" \"${i.funcName}\" (func ${i.ident.show} ${paramStr}${resultStr}))")

extension (mod: Module)
  def show: String =
    val printer = WasmPrinter()
    printer.printModule(mod)
    printer.result()
