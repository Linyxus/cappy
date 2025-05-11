import cavia.*
import io.*
import tokenizing.*
import parsing.*
import reporting.*
import typechecking.*
import core.ast.*
import Printer.*
import codegen.*
import java.nio.file.*

@main def runCompiler(sourcePaths: String*): Unit =
  if sourcePaths.length < 1 then
    println("Expecting at least one argument")
    return

  val sources = sourcePaths.toList.map(SourceFile.fromPath) :+ Compiler.stdlib
  println(s"--- input file(s)")
  sources.foreach: source =>
    println(source.content)
  val result = Compiler.parseAll(sources)
  result match
    case Left(err: Tokenizer.Error) =>
      if err.hasPos then
        println(Printer.showSourcePos(err.pos, List(err.toString)))
      else
        println(s"Tokenization error: $err")
    case Left(err: Parser.ParseError) =>
      println(err.show)
    case Right(parsedModules) =>
      println(s"--- tree after parser")
      parsedModules.foreach: parsedModule =>
        println(parsedModule)
      println(s"--- tree after typechecker")
      val mods = TypeChecker.checkModules(parsedModules)(using TypeChecker.Context.empty)
      mods match
        case Left(err) => 
          println(err.asMessage.show)
        case Right(mods) =>
          mods.foreach: mod =>
            println(ExprPrinter.show(mod)(using TypeChecker.Context.empty))
          given genCtx: CodeGenerator.Context = CodeGenerator.Context()
          CodeGenerator.genModules(mods)
          val wasmMod = CodeGenerator.finalize
          val outputCode = wasmMod.show
          println(s"--- wasm module")
          // println(outputCode)
          println(s"... too long to print ...")

          val inputPath = Paths.get(sourcePaths(0))
          val outputName = inputPath.getFileName.toString.replace(".scala", ".wat")
          val outputPath = inputPath.getParent.resolve(outputName)
          Files.writeString(outputPath, outputCode)
          println(s"--- wrote to $outputPath")
