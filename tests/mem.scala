def main(): Unit =
  WASM_MEMORY(0) = 42
  val i = WASM_MEMORY(0)
  #i32println(i)
  #i32println(WASM_MEMORY.size)

