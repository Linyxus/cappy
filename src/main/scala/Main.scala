import cavia.*
import io.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*
import codegen.*
import java.nio.file.*

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
      println(s"--- tree after typechecker")
      val mod = TypeChecker.checkModule(result)(using TypeChecker.Context.empty)
      mod match
        case Left(err) => 
          println(Printer.showSourcePos(err.pos, List(err.toString)))
        case Right(mod) =>
          println(ExprPrinter.show(mod)(using TypeChecker.Context.empty))
          given genCtx: CodeGenerator.Context = CodeGenerator.Context()
          CodeGenerator.genModule(mod)
          val wasmMod = CodeGenerator.finalize
          val outputCode = wasmMod.show
          println(s"--- wasm module")
          println(outputCode)

          val inputPath = Paths.get(path)
          val outputName = inputPath.getFileName.toString.replace(".scala", ".wat")
          val outputPath = inputPath.getParent.resolve(outputName)
          Files.writeString(outputPath, outputCode)
          println(s"--- wrote to $outputPath")
