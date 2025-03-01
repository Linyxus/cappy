import cappy.*
import core.*
import io.*
import tokenizer.*
import reporting.*
import cappy.reporting.Printer.showSourcePos

@main def hello(): Unit =
  val source = SourceFile("test", """
val x = (x: Int) =>
  x("hello, world)
val y = (y: Int) =>
  x(y)
""")
  val tokens = Tokenizer.tokenize(source)
  tokens.foreach: token =>
    val msg = if token.hasPos then showSourcePos(token.pos, List(token.toString)) else "NOPOS:" + token.toString
    println(msg)
