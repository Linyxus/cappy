import cappy.*
import io.*
import reporting.*
import Printer.*

@main def hello(): Unit =
  val source = SourceFile("test", """
val swap: [cap C1, C2] -> [T1, T2] -> (x: Ref[T1]^{C1}, y: Ref[T2]^{C2}) -> Unit = "???"
val x1: Ref[Int]^{cap} = newRef()
val x2: Ref[Int]^{cap} = newRef()
val test: () -> Unit = () =>
  swap[cap {x1}, {x1}][Int, Int](x1, x1)
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
