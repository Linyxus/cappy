package cappy

import io.*
import core.ast.*
import tokenizing.*
import parsing.*
import reporting.*
import Printer.*

object Compiler:
  enum ParseResult:
    case TokenizationError(err: Tokenizer.Error)
    case ParsingError(err: Parser.ParseError)
    case Ok(result: List[Syntax.Definition])

  def parse(source: SourceFile): ParseResult =
    val tokens = Tokenizer.tokenize(source)
    val errTokens = tokens.collect { case err: Tokenizer.Error => err }
    if errTokens.nonEmpty then
      ParseResult.TokenizationError(errTokens.head)
    else
      // println("Tokenization successful")
      val tokenArray = (tokens.collect { case token: Token => token }).toArray
      // tokenArray.foreach: token =>
      //   println(Printer.showSourcePos(token.pos, List(token.toString)))
      val state = Parser.ParserState(tokenArray, 0)
      val ctx = Parser.ParserContext.empty
      Parsers.programP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          val errs = ctx.errors.sortBy(_.pos.span.end)
          val farestErr = errs.last
          ParseResult.ParsingError(farestErr)
        case Parser.ParseResult(nextState, Right(result)) =>
          ParseResult.Ok(result)

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
      Parsers.definitionP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          println(err.show)
        case Parser.ParseResult(nextState, Right(result)) =>
          println("Parsing successful")
          println(Printer.showSourcePos(result.pos, List(result.toString)))
