import cavia.*
import io.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*

@main def hello(path: String): Unit =
  val source = SourceFile.fromPath(path)
  println(s"--- input file")
  println(source.content)
  val result = Compiler.parse(source)
  result match
    case Compiler.ParseResult.TokenizationError(err) =>
      if err.hasPos then
        println(Printer.showSourcePos(err.pos, List(err.toString)))
      else
        println(s"Tokenization error: $err")
    case Compiler.ParseResult.ParsingError(err) =>
      println(err.show)
    case Compiler.ParseResult.Ok(result) =>
      println(s"--- tree after parser")
      result.foreach(println)
      val mod = TypeChecker.checkModule(result)(using TypeChecker.Context.empty)
      mod match
        case Left(err) => 
          println(Printer.showSourcePos(err.pos, List(err.toString)))
        case Right(mod) =>
          println(s"--- tree after typechecker")
          println(ExprPrinter.show(mod)(using TypeChecker.Context.empty))
