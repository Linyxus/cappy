package cavia
package typechecking

import core.ast.*
import reporting.IndentedPrinter
import Expr.*
import TypeChecker.*

class ExprPrinter extends IndentedPrinter:
  def show(t: Expr.Term)(using Context): Unit = 
    t match
      case Term.BinderRef(idx) => print(getBinder(idx).name)
      case Term.SymbolRef(sym) => print(sym.name)
      case Term.StrLit(value) => print(s""""$value"""")
      case Term.IntLit(value) => print(value.toString)
      case Term.UnitLit() => print("()")
      case Term.TermLambda(params, body) =>
        print("(")
        def showBinders(bds: List[Binder])(using Context): List[String] = bds match
          case Nil => Nil
          case p :: ps =>
            val s = TypePrinter.show(p)
            s :: showBinders(ps)(using ctx.extend(p))
        print(showBinders(params).mkString(", "))
        print(") => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.TypeLambda(params, body) =>
        print("[")
        def showBinders(bds: List[Binder])(using Context): List[String] = bds match
          case Nil => Nil
          case p :: ps =>
            val s = TypePrinter.show(p)
            s :: showBinders(ps)(using ctx.extend(p))
        print(showBinders(params).mkString(", "))
        print("] => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.Bind(binder, expr, body) =>
        print("val ")
        print(binder.name)
        print(": ")
        showType(binder.tpe)
        print(" = ")
        newline()
        indented:
          show(expr)
        newline()
        show(body)(using ctx.extend(binder))
      case Term.PrimOp(op, args) =>
        print(op.toString)
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

  def showType(tpe: Type)(using Context): Unit =
    print(TypePrinter.show(tpe))

  def showModule(mod: Module)(using Context): Unit =
    print("module {")
    newline()
    indented:
      mod.defns.foreach(showDefinition)
    println("}")

  def showDefinition(defn: Definition)(using Context): Unit =
    defn match
      case Definition.ValDef(name, tpe, expr) =>
        print("val ")
        print(name.name)
        print(": ")
        showType(tpe)
        print(" = ")
        newline()
        indented:
          show(expr)
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
    