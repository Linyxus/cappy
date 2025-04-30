val PAGE_SIZE: i32 = 65535
val I32_SIZE: i32 = 4
val HEAD_SIZE: i32 = 2 * I32_SIZE

extension (ptr: i32)
  def size: i32 = WASM_MEMORY(ptr)
  def next: i32 = WASM_MEMORY(ptr + I32_SIZE)
  def setSize(size: i32): Unit = WASM_MEMORY(ptr) = size
  def setNext(next: i32): Unit = WASM_MEMORY(ptr + I32_SIZE) = next

// Init memory state
val init: Unit =
  WASM_MEMORY(0) = I32_SIZE
  WASM_MEMORY(1 * I32_SIZE) = PAGE_SIZE - 1
  WASM_MEMORY(2 * I32_SIZE) = -1
def getHead(): i32 = WASM_MEMORY(0)
def setHead(new: i32): Unit = WASM_MEMORY(0) = new

// Try to allocate a memory buffer of `size`.
// If fail, return -1.
def alloc(size: i32): i32 =
  def go(current: i32): i32 =
    if current < 0 then -1
    else
      if current.size >= size then
        val newSize = current.size - size
        val newNext = current + size
        current.setSize(size)
        setHead(newNext)
        newNext.setSize(newSize)
        newNext.setNext(current.next)
        current + HEAD_SIZE
      else go(current.next)
  go(getHead())
// Deallocate a block
def free(ptr: i32): Unit =
  val ptr = ptr - HEAD_SIZE
  ptr.setNext(getHead())
  setHead(ptr)

def debug(): Unit =
  def go(ptr: i32): Unit =
    if ptr >= 0 then
      #i32println(ptr.size)
      #i32println(ptr.next)
      #i32println(9999999)
      go(ptr.next)
  go(getHead())

def main(): Unit =
  val ptr1 = alloc(256)
  val ptr2 = alloc(32)
  free(ptr1)
  debug()

