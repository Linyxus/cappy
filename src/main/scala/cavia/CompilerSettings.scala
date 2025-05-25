package cavia

import io.*

object CompilerSettings:
  enum CompilerAction:
    case Check(sourceFiles: List[SourceFile])
    case Codegen(sourceFiles: List[SourceFile])
    case Help

  case class CompilerConfig(
    printIds: Boolean,
  ):
    def turnOffPrintIds: CompilerConfig = copy(printIds = false)

  object CompilerConfig:
    val default: CompilerConfig = CompilerConfig(
      printIds = true,
    )
