package cavia

import io.*
import core.ast.*
import core.*
import CompilerSettings.*
import tokenizing.*
import parsing.*
import reporting.*
import Printer.*
import typechecking.*
import codegen.*
import java.nio.file.*

object Compiler:
  object ParseStep extends CompilerStep[List[SourceFile], List[Syntax.Module]]:
    def run(sources: List[SourceFile]): Outcome[List[Syntax.Module]] =
      parseAll(sources) match
        case Left(err: Tokenizer.Error) => Outcome.simpleFailure(err.asMessage)
        case Left(err: Parser.ParseError) => Outcome.simpleFailure(Message.simple(err.show, err.pos))
        case Right(modules) => Outcome.simpleSuccess(modules)

  object TypecheckStep extends CompilerStep[List[Syntax.Module], List[Expr.Module]]:
    def run(sources: List[Syntax.Module]): Outcome[List[Expr.Module]] =
      val ctx = TypeChecker.Context.empty
      TypeChecker.checkModules(sources)(using ctx) match
        case Left(errs) =>
          new Outcome:
            def getOption = None
            def getMessages = errs.map(_.asMessage)
        case Right(res) => Outcome.simpleSuccess(res)

  class PrintModulesStep(after: String) extends CompilerStep[List[Expr.Module], List[Expr.Module]]:
    def run(modules: List[Expr.Module]): Outcome[List[Expr.Module]] =
      println(s"--- modules after $after")
      modules.foreach: m =>
        if m.name != "std" then
          println(ExprPrinter.show(m)(using TypeChecker.Context.empty))
      Outcome.simpleSuccess(modules)

  class CodegenStep(outputPath: String) extends CompilerStep[List[Expr.Module], Unit]:
    def run(modules: List[Expr.Module]): Outcome[Unit] =
      given genCtx: CodeGenerator.Context = CodeGenerator.Context()
      CodeGenerator.genModules(modules)
      val wasmMod = CodeGenerator.finalize
      val outputCode = wasmMod.show
      // println("--- wasm module")
      // println(outputCode)
      Files.writeString(Path.of(outputPath), outputCode)
      Outcome.simpleSuccess(())

  def parseOptions(args: List[String]): Option[CompilerAction] =
    args match
      case Nil => Some(CompilerAction.Help)
      case "check" :: sources => Some(CompilerAction.Check(sources.map(SourceFile.fromPath)))
      case "gen" :: sources => Some(CompilerAction.Codegen(sources.map(SourceFile.fromPath)))
      case _ => None

  def run(action: CompilerAction): Unit =
    action match
      case CompilerAction.Help =>
        println("Usage: cavia check <source files>")
        println("       cavia gen <source files>")
      case CompilerAction.Check(sources) =>
        val sources1 = sources :+ stdlib
        val runner = ParseStep `fuse` TypecheckStep `fuse` PrintModulesStep("typecheck")
        val res = runner.execute(sources1)
        res match
          case Some(modules) =>
            println("--- typechecked modules")
            modules.init.foreach: m =>
              println(ExprPrinter.show(m)(using TypeChecker.Context.empty))
          case None =>
      case CompilerAction.Codegen(sources) =>
        val runner = 
          ParseStep 
            `fuse` TypecheckStep 
            `fuse` PrintModulesStep("typecheck")
            `fuse` CodegenStep("out.wat")
        val res = runner.execute(sources :+ stdlib)
        res match
          case Some(_) =>
            println("--- codegen successful")
          case None =>
            println("--- codegen failed")

  type ParseResult[+X] = Either[Tokenizer.Error | Parser.ParseError, X]

  def parseAll(sources: List[SourceFile]): ParseResult[List[Syntax.Module]] =
    hopefully:
      val modules = sources.map(parse(_).!!)
      (sources `zip` modules).foreach: (source, module) =>
        module.sourceFile = source
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
// Counter
struct Counter(var count: i32)
extension (c: Counter^)
  def inc(): Unit =
    c.count = c.get() + 1
  def get(): i32 = c.count
// Range
struct Range(start: i32, end: i32)
extension (i: i32)
  def until(end: i32): Range^ = Range(i, end)
extension (r: Range^)
  def iterate(op: i32 => Unit): Unit =
    val cnt = Counter(r.start)
    def loop(): Unit =
      if cnt.get() < r.end then
        op(cnt.get())
        cnt.inc()
        loop()
    loop()
// Array
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
// String and IO
type String = array[char]
def putChar(ch: char): Unit = #putchar(ch)
def putStr(s: String^): Unit =
  s.foreach(putChar)
def putStrLn(s: String^): Unit =
  putStr(s)
  putChar('\n')
extension (self: String^)
  def concat(other: String^): String^ =
    val res = newArray[char](self.size + other.size, ' ')
    (0.until(self.size)).iterate((i: i32) => res(i) = self(i))
    (0.until(other.size)).iterate((i: i32) => res(i + self.size) = other(i))
    res
// Performance counter
def perfCounter(): i32 = #perfcounter()
def benchmark(body: () => Unit): i32 =
  val start = perfCounter()
  body()
  val end = perfCounter()
  end - start
""")
