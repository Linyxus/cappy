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
        current.setSize(size)
        if newSize == 0 then
          setHead(current.next)
        else
          val newNext = current + size
          setHead(newNext)
          newNext.setSize(newSize)
          newNext.setNext(current.next)
        current + HEAD_SIZE
      else go(current.next)
  go(getHead())
// Deallocate a block
def free(ptr: i32): Unit =
  def isAdj(p1: i32, p2: i32): bool =
    (p1 + p1.size) == p2
  def go(prevPtr: i32, curPtr: i32, accPtr: i32): Unit =
    if curPtr < 0 then
      // Adjecent block not found in free list.
      // Simply prepend the freed block to the free list
      if prevPtr < 0 then
        accPtr.setNext(-1)
      else
        accPtr.setNext(getHead())
      setHead(accPtr)
    else if isAdj(curPtr, accPtr) then
      // [ current ] [ acc ]
      curPtr.setSize(curPtr.size + accPtr.size)
      if prevPtr >= 0 then
        prevPtr.setNext(curPtr.next)
      go(prevPtr, curPtr.next, curPtr)
    else if isAdj(accPtr, curPtr) then
      // [ acc ] [ current ]
      accPtr.setSize(accPtr.size + curPtr.size)
      if prevPtr >= 0 then
        prevPtr.setNext(curPtr.next)
      go(prevPtr, curPtr.next, accPtr)
    else
      // Non-adjecent block
      go(curPtr, curPtr.next, accPtr)
  go(-1, getHead(), ptr - HEAD_SIZE)

def debug(): Unit =
  def go(ptr: i32): Unit =
    if ptr >= 0 then
      #i32println(ptr.size)
      #i32println(ptr.next)
      #i32println(1234567890)
      go(ptr.next)
  go(getHead())

def main(): Unit =
  val ptr1 = alloc(256)
  val ptr2 = alloc(32)
  //val ptr3 = alloc(1024)
  //val ptr4 = alloc(10000)
  free(ptr1)
  free(ptr2)
  //free(ptr3)
  //free(ptr4)
  debug()

