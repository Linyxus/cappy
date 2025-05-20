type Ptr = i32
val WORD_SIZE: i32 = 4
val PAGE_SIZE: i32 = 65536
val BUCKET4_HEAD: Ptr = 0
val BUCKET8_HEAD: Ptr = BUCKET4_HEAD + 4
val BUCKET16_HEAD: Ptr = BUCKET8_HEAD + 4
val BUCKET24_HEAD: Ptr = BUCKET16_HEAD + 4
val BUCKET32_HEAD: Ptr = BUCKET24_HEAD + 4
val BUCKETX_HEAD: Ptr = BUCKET32_HEAD + 4

def getNext(block: Ptr): i32 = WASM_MEMORY(block)
def getSize(block: Ptr): i32 = WASM_MEMORY(block + 4)
def setNext(block: Ptr, next: Ptr): Unit = WASM_MEMORY(block) = next
def setSize(block: Ptr, size: i32): Unit = WASM_MEMORY(block + 4) = size

val __malloc_init: Unit =
  WASM_MEMORY(BUCKET4_HEAD) = -1
  WASM_MEMORY(BUCKET8_HEAD) = -1
  WASM_MEMORY(BUCKET16_HEAD) = -1
  WASM_MEMORY(BUCKET24_HEAD) = -1
  WASM_MEMORY(BUCKET32_HEAD) = -1
  // Init the first block
  val firstBlock: Ptr = BUCKETX_HEAD + 4
  setNext(firstBlock, -1)
  val initSize = PAGE_SIZE - firstBlock - 8
  setSize(firstBlock, initSize)
  WASM_MEMORY(BUCKETX_HEAD) = firstBlock

def alignSize(requestedSize: i32): i32 =
  ((requestedSize - 1) / WORD_SIZE + 1) * WORD_SIZE

def initBlock(ptr: Ptr, next: Ptr, size: i32): Unit =
  setNext(ptr, next)
  setSize(ptr, size)

def findBlock(head: Ptr, size: i32): Ptr =
  if head < 0 then -1
  else
    val thisSize = getSize(head)
    if thisSize == size then
      sorry()
    else if thisSize > size then
      sorry()
    else findBlock(getNext(head), size)

def alloc(size: i32): Ptr =
  -1

def main(): Unit =
  putStrLn("hello")

