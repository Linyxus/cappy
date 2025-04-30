package cavia
package typechecking

import core.ast.*
import reporting.IndentedPrinter
import Expr.*
import TypeChecker.*

class ExprPrinter extends IndentedPrinter:
  def showBinders(bds: List[Binder])(using Context): List[String] = bds match
    case Nil => Nil
    case p :: ps =>
      val s = TypePrinter.show(p)
      s :: showBinders(ps)(using ctx.extend(p))

  def show(t: Expr.Term)(using Context): Unit = 
    t match
      case Term.BinderRef(idx) => print(getBinder(idx).name)
      case Term.SymbolRef(sym) => print(sym.name)
      case Term.StrLit(value) => print(s""""$value"""")
      case Term.IntLit(value) => print(value.toString)
      case Term.BoolLit(value) => print(value.toString)
      case Term.UnitLit() => print("()")
      case Term.TermLambda(params, body, _) =>
        print("(")
        print(showBinders(params).mkString(", "))
        print(") => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.Select(base, fieldInfo) =>
        show(base)
        print(".")
        print(fieldInfo.name)
      case Term.TypeLambda(params, body) =>
        print("[")
        print(showBinders(params).mkString(", "))
        print("] => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.Bind(binder, recursive, expr, body) =>
        print("val ")
        print(binder.name)
        print(": ")
        showType(binder.tpe)
        print(" = ")
        newline()
        indented:
          val ctx1 = if recursive then ctx.extend(binder) else ctx
          show(expr)(using ctx1)
        newline()
        show(body)(using ctx.extend(binder))
      case Term.PrimOp(op, targs, args) =>
        print(op.toString)
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.StructInit(sym, targs, args) =>
        print(sym.name)
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.Apply(fun, args) =>
        show(fun)
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.TypeApply(term, targs) =>
        show(term)
        print("[")
        targs.zipWithIndex.foreach: (targ, idx) =>
          targ match
            case targ: CaptureSet =>
              print(TypePrinter.show(targ))
            case targ: Type =>
              print(TypePrinter.show(targ))
          if idx < targs.size - 1 then
            print(", ")
        print("]")
      case Term.If(cond, thenBranch, elseBranch) =>
        print("if ")
        show(cond)
        print(" then")
        newline()
        indented:
          show(thenBranch)
        newline()
        print("else")
        newline()
        indented:
          show(elseBranch)
      case Term.ResolveExtension(sym, targs, methodName) =>
        print(s"extension(${sym.name})")
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print(".")
        print(methodName)
  def showType(tpe: Type | CaptureSet)(using Context): Unit =
    tpe match
      case tpe: Type =>
        print(TypePrinter.show(tpe))
      case cs: CaptureSet =>
        print(TypePrinter.show(cs))

  def showModule(mod: Module)(using Context): Unit =
    print("module {")
    newline()
    indented:
      mod.defns.foreach(showDefinition)
    println("}")

  def showDefinition(defn: Definition)(using Context): Unit =
    defn match
      case Definition.ValDef(sym, expr) =>
        print("val ")
        print(sym.name)
        print(": ")
        showType(sym.tpe)
        print(" = ")
        newline()
        indented:
          show(expr)
        newline()
      case Definition.StructDef(sym) =>
        print("struct ")
        print(sym.name)
        val binders = sym.info.targs
        val binderStrs = showBinders(binders)
        if binders.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        print("(")
        val ctx1 = ctx.extend(binders)
        sym.info.fields.zipWithIndex.foreach: (field, idx) =>
          if field.mutable then
            print("var ")
          print(field.name)
          print(": ")
          showType(field.tpe)(using ctx1)
          if idx < sym.info.fields.size - 1 then
            print(", ")
        print(")")
        newline()
      case Definition.ExtensionDef(sym) =>
        print("extension ")
        print(sym.name)
        val info = sym.info
        val binderStrs = showBinders(info.typeParams)
        if info.typeParams.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        val ctx1 = ctx.extend(info.typeParams)
        print("(?: ")
        print(TypePrinter.show(info.selfArgType)(using ctx1))
        print(") {")
        newline()
        indented:
          info.methods.foreach: method =>
            print("val ")
            print(method.name)
            print(": ")
            showType(method.tpe)(using ctx1)
            print(" = ")
            newline()
            indented:
              show(method.body)(using ctx1)
            newline()
        newline()
        print("}")
        newline()

object ExprPrinter:
  def show(t: Expr.Term)(using Context): String =
    val printer = ExprPrinter()
    printer.show(t)
    printer.result()

  def show(mod: Module)(using Context): String =
    val printer = ExprPrinter()
    printer.showModule(mod)
    printer.result()
    