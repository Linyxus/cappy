import cappy.*
import core.*
import io.*
import tokenizing.*
import reporting.*
import Printer.*
import parsing.*
import Parser.*
import Parsers.*

@main def hello(): Unit =
  val source = SourceFile("test", """
[cap C, D] -> Int
""".strip())
  val tokens = Tokenizer.tokenize(source)
  tokens.foreach: token =>
    val msg = if token.hasPos then showSourcePos(token.pos, List(token.toString)) else "NOPOS:" + token.toString
    println(msg)
  val tokenArray = (tokens.collect { case token: Token => token }).toArray
  val state = ParserState(tokenArray, 0)
  captureArrowP.runParser(state) match
    case ParseResult(nextState, Left(err)) => println(err.show)
    case ParseResult(nextState, Right(result)) =>
      println(Printer.showSourcePos(result.pos, List(result.toString)))
  