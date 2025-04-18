import cappy.*
import io.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val t: Unit = int_add
""")
  val result = Compiler.parse(source)
  result match
    case Compiler.ParseResult.TokenizationError(err) =>
      println(Printer.showSourcePos(err.pos, List(err.toString)))
    case Compiler.ParseResult.ParsingError(err) =>
      println(err.show)
    case Compiler.ParseResult.Ok(result) =>
      result.foreach: defn =>
        //println(Printer.showSourcePos(defn.pos, List(defn.toString)))
        defn match
          case Syntax.Definition.ValDef(name, Some(tpe), body) =>
            println(s"val $name: $tpe = $body")
            val res = TypeChecker.checkType(tpe)(using TypeChecker.Context.empty)
            res match
              case Right(tpe) =>
                println(Printer.showSourcePos(tpe.pos, List("success!", tpe.toString)))
                val res2 = TypeChecker.checkTerm(body)(using TypeChecker.Context.empty)
                res2 match
                  case Right(term) =>
                    println(Printer.showSourcePos(term.pos, List("success!", term.tpe.toString)))
                    val res3 = TypeComparer.checkSubtype(term.tpe, tpe)(using TypeChecker.Context.empty)
                    println(s"subtype check: $res3")
                  case Left(err) =>
                    println(Printer.showSourcePos(err.pos, List("error!", err.toString)))
              case Left(err) =>
                println(Printer.showSourcePos(err.pos, List("error!", err.toString)))
          case _ =>
