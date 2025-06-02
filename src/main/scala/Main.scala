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
  if !Binaryen.checkTools then
    println("Please install the Binaryen toolchain (https://github.com/WebAssembly/binaryen).")
  else
    val args = sourcePaths.toList
    val action = Compiler.parseOptions(args)
    action match
      case Left(err) =>
        println(err)
      case Right(action) =>
        Compiler.run(action)
