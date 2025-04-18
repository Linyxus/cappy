import cappy.*
import io.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val x: i64 = 1
val y: i64 = x
def add(x: i64, y: i64): i64 = #i64add(x, y)
def identity[T](x: T): T = x
val z: i64 = #i64add(x, z)
""")
  val result = Compiler.parse(source)
  result match
    case Compiler.ParseResult.TokenizationError(err) =>
      println(Printer.showSourcePos(err.pos, List(err.toString)))
    case Compiler.ParseResult.ParsingError(err) =>
      println(err.show)
    case Compiler.ParseResult.Ok(result) =>
      result.foreach(println)
      val mod = TypeChecker.checkModule(result)(using TypeChecker.Context.empty)
      mod match
        case Left(err) => 
          println(Printer.showSourcePos(err.pos, List(err.toString)))
        case Right(mod) =>
          println(ExprPrinter.show(mod)(using TypeChecker.Context.empty))

      // result.foreach: defn =>
        //println(Printer.showSourcePos(defn.pos, List(defn.toString)))
        // defn match
        //   case Syntax.Definition.ValDef(name, Some(tpe), body) =>
        //     println(s"val $name: $tpe = $body")
        //     val res = TypeChecker.checkType(tpe)(using TypeChecker.Context.empty)
        //     res match
        //       case Right(tpe) =>
        //         println(Printer.showSourcePos(tpe.pos, List("success!", tpe.toString)))
        //         val res2 = TypeChecker.checkTerm(body)(using TypeChecker.Context.empty)
        //         res2 match
        //           case Right(term) =>
        //             println(Printer.showSourcePos(term.pos, List("success!", term.tpe.toString)))
        //             val res3 = TypeComparer.checkSubtype(term.tpe, tpe)(using TypeChecker.Context.empty)
        //             println(s"subtype check: $res3")
        //           case Left(err) =>
        //             println(Printer.showSourcePos(err.pos, List("error!", err.toString)))
        //       case Left(err) =>
        //         println(Printer.showSourcePos(err.pos, List("error!", err.toString)))
        //   case Syntax.Definition.DefDef(name, params, resultType, body) =>
        //     println(Printer.showSourcePos(defn.pos, List(defn.toString)))
        //   case _ =>
