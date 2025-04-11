import cappy.*
import io.*
import reporting.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val foo: [cap C] -> U = test
val bar: [cap C, X <: IO^{C}, cap D <: {C}] -> U = test
""")
  val result = Compiler.parse(source)
  result match
    case Compiler.ParseResult.TokenizationError(err) =>
      println(Printer.showSourcePos(err.pos, List(err.toString)))
    case Compiler.ParseResult.ParsingError(err) =>
      println(err.show)
    case Compiler.ParseResult.Ok(result) =>
      result.foreach: defn =>
        println(Printer.showSourcePos(defn.pos, List(defn.toString)))
