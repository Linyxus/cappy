import cappy.*
import io.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val t: (x: Int) -> Int^{x} = ()
val t: (x: Int) -> Int^{cap} = ()
val t: (x: Int, y: Int^{x}) -> Int^{x,y} = ()
val t: [X, cap C] -> (x: X^{C}, y: Int^{x}) -> Int^{x,y} = ()
val t: [X <: (x: Int) -> Int^{x}, cap C] -> (x: X^{C}, y: Int^{x}) -> Int^{x,y} = ()
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
            println(s"val $name: $tpe")
            val res = TypeChecker.checkType(tpe)(using TypeChecker.Context.empty)
            res match
              case Right(tpe) =>
                println(Printer.showSourcePos(tpe.pos, List("success!", tpe.toString)))
              case Left(err) =>
                println(Printer.showSourcePos(err.pos, List("error!", err.toString)))
          case _ =>
