import cappy.*
import io.*
import reporting.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val swap: [cap C1, C2] -> (x: Ref^{C1}, y: Ref^{C2}) -> Unit = 0
val x1: Ref^{cap} = ()
val x2: Ref^{cap} = newRef()
val test: () -> Unit = () =>
  swap[cap {x1}, {x1}](x1, x1)
  swap(x1, x2)
def test(): Unit =
  swap[cap {x1}, {x1}](x1, x1)
  swap(x1, x2)
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
