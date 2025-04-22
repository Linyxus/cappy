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

  def printFunc(func: Func): Unit =
    print(s"(func ${func.ident.show} ")
    func.params.foreach: (paramId, paramType) =>
      print(s"(param ${paramId.show} ${paramType.show}) ")
    print(s"(result ${func.result.show})")
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
    newline()

extension (mod: Module)
  def show: String =
    val printer = WasmPrinter()
    printer.printModule(mod)
    printer.result()
