package cavia

import io.*
import core.ast.*
import tokenizing.*
import parsing.*
import reporting.*
import Printer.*
import typechecking.*

object Compiler:
  type ParseResult[+X] = Either[Tokenizer.Error | Parser.ParseError, X]

  def parseAll(sources: List[SourceFile]): ParseResult[List[Syntax.Module]] =
    hopefully:
      val modules = sources.map(parse(_).!!)
      modules

  def parse(source: SourceFile): ParseResult[Syntax.Module] =
    val tokens = Tokenizer.tokenize(source)
    val errTokens = tokens.collect { case err: Tokenizer.Error => err }
    if errTokens.nonEmpty then
      Left(errTokens.head)
    else
      // println("Tokenization successful")
      val tokenArray = (tokens.collect { case token: Token => token }).toArray
      // tokenArray.foreach: token =>
      //   println(Printer.showSourcePos(token.pos, List(token.toString)))
      val state = Parser.ParserState(tokenArray, 0)
      val ctx = Parser.ParserContext.empty
      Parsers.moduleP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          val errs = ctx.errors.sortBy(_.pos.span.end)
          // println("Errors:")
          // errs.foreach: err =>
          //   println(Printer.showSourcePos(err.pos, List(err.toString)))
          val farestErr = if errs.nonEmpty then errs.last else err
          Left(farestErr)
        case Parser.ParseResult(nextState, Right(result)) =>
          Right(result)

  def compile(source: SourceFile): Unit =
    val tokens = Tokenizer.tokenize(source)
    val errTokens = tokens.collect { case err: Tokenizer.Error => err }
    if errTokens.nonEmpty then
      println("Tokenization errors:")
      errTokens.foreach: err =>
        println(Printer.showSourcePos(err.pos, List(err.toString)))
    else
      val tokenArray = (tokens.collect { case token: Token => token }).toArray
      println("Tokenization successful")
      tokenArray.foreach: token =>
        println(Printer.showSourcePos(token.pos, List(token.toString)))
      val state = Parser.ParserState(tokenArray, 0)
      val ctx = Parser.ParserContext.empty
      Parsers.moduleP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          println(err.show)
        case Parser.ParseResult(nextState, Right(result)) =>
          println("Parsing successful")
          println(Printer.showSourcePos(result.pos, List(result.toString)))

  val stdlib: SourceFile = SourceFile(
    name = "<stdlib>",
    content = """
module std
extension [T](xs: array[T]^)
  def mapInPlace(f: T => T): Unit =
    def loop(i: i32): Unit =
      if i < xs.length then
        xs(i) = f(xs(i))
        loop(i+1)
    loop(0)
  def foreach(f: T => Unit): Unit =
    def op(x: T): T =
      f(x)
      x
    xs.mapInPlace(op)
type String = array[char]
def putChar(ch: char): Unit = #putchar(ch)
def putStr(s: String^): Unit =
  s.foreach(putChar)
def putStrLn(s: String^): Unit =
  putStr(s)
  putChar('\n')
"""
  )
