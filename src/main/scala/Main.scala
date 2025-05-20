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
  val args = sourcePaths.toList
  val action = Compiler.parseOptions(args)
  action match
    case None =>
      println("Invalid arguments")
    case Some(action) =>
      Compiler.run(action)
