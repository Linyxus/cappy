package cavia.io
import ShellUtils.*

object WasmAssembler:
  val defaultFlags: List[String] = List(
    "--enable-reference-types",
    "--enable-gc",
    "--enable-tail-call",
    "--enable-multimemory",
  )
  def exists: Boolean = executableExists("wasm-as")

  def compile(input: String, output: String, flags: List[String] = defaultFlags): ShellResult =
    val cmd = Seq("wasm-as", input, "-o", output) ++ flags
    execute(cmd*)

object WasmOptimizer:
  val defaultFlags: List[String] = WasmAssembler.defaultFlags ++ List(
    "-O4"
  )

  def optimize(input: String, output: String, flags: List[String] = defaultFlags): ShellResult =
    val cmd = Seq("wasm-opt", input, "-o", output) ++ flags
    execute(cmd*)

  def exists: Boolean = executableExists("wasm-opt")

object Binaryen:
  def checkTools: Boolean =
    val checkAssembler: Boolean =
      if WasmAssembler.exists then
        true
      else
        println("wasm-as not found")
        false
    val checkOptimizer: Boolean =
      if WasmOptimizer.exists then
        true
      else
        println("wasm-opt not found")
        false
    checkAssembler && checkOptimizer
