package cavia

import io.*

object CompilerSettings:
  enum CompilerAction:
    /** Typecheck the source files. */
    case Check(sourceFiles: List[SourceFile])
    /** Typecheck source files and generate .wat code. */
    case Codegen(sourceFiles: List[SourceFile])
    /** Typecheck, generate .wat code, then compile to an optimized .wasm binary. */
    case Compile(sourceFiles: List[SourceFile])
    /** Print the help message. */
    case Help

  case class CompilerConfig(
    printIds: Boolean,
  ):
    def turnOffPrintIds: CompilerConfig = copy(printIds = false)

  object CompilerConfig:
    val default: CompilerConfig = CompilerConfig(
      printIds = true,
    )
