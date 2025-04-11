import cappy.*
import io.*
import reporting.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
def foo[cap C](ops: List[() ->{C} Unit]): Unit = test
def bar[cap C, X](comps: List[() ->{C} X]): List[X] = test
def baz[cap C, cap D, A, B](f: (z: A) ->{C} B, g: (z: B) ->{D} A) = test
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
