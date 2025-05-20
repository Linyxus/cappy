type Ptr = i32
val WORD_SIZE: i32 = 4
val PAGE_SIZE: i32 = 65536
val NUM_SMALL_BUCKETS: i32 = 64
def getBucketHead(size: i32): i32 =
  if size <= NUM_SMALL_BUCKETS * 4 then
    size - 4
  else bucketXHead
val bucketXHead: i32 = NUM_SMALL_BUCKETS * 4

def getNext(block: Ptr): i32 = WASM_MEMORY(block)
def getSize(block: Ptr): i32 = WASM_MEMORY(block + 4)
def setNext(block: Ptr, next: Ptr): Unit = WASM_MEMORY(block) = next
def setSize(block: Ptr, size: i32): Unit = WASM_MEMORY(block + 4) = size

val __malloc_init: Unit =
  def initLoop(cur: i32): Unit =
    if cur <= NUM_SMALL_BUCKETS then
      val bucketSize = cur * 4
      val head = getBucketHead(bucketSize)
      WASM_MEMORY(head) = -1
      initLoop(cur+1)
  initLoop(1)
  // Init the first block
  val firstBlock: Ptr = bucketXHead + 4
  setNext(firstBlock, -1)
  val initSize = PAGE_SIZE - firstBlock - 8
  setSize(firstBlock, initSize)
  WASM_MEMORY(bucketXHead) = firstBlock

def alignSize(requestedSize: i32): i32 =
  ((requestedSize - 1) / WORD_SIZE + 1) * WORD_SIZE

def initBlock(ptr: Ptr, next: Ptr, size: i32): Unit =
  setNext(ptr, next)
  setSize(ptr, size)

def findBlock(nextPtr: Ptr, size: i32): Ptr =
  val head: Ptr = WASM_MEMORY(nextPtr)
  if head < 0 then -1
  else
    val thisSize = getSize(head)
    if thisSize == size then
      WASM_MEMORY(nextPtr) = getNext(head)
      head + 8
    else if thisSize > size + 8 then
      val newBlockPtr = head + 8 + size
      initBlock(newBlockPtr, getNext(head), thisSize - size - 8)
      initBlock(head, -1, size)
      WASM_MEMORY(nextPtr) = newBlockPtr
      head + 8
    else findBlock(head, size)

def getPtrOrElse(p: Ptr)(orElse: () => Ptr): Ptr =
  if p < 0 then orElse()
  else p

def alloc(size: i32): Ptr =
  val blockSize = alignSize(size)
  val selectedBucket = selectBucket(blockSize)
  getPtrOrElse(findBlock(selectedBucket, blockSize)): () =>
    if selectedBucket != bucketXHead then
      findBlock(bucketXHead, blockSize)
    else -1

def appendBlock(bucketPtr: Ptr, head: i32): Unit =
  setNext(head, WASM_MEMORY(bucketPtr))
  WASM_MEMORY(bucketPtr) = head

def free(ptr: i32): Unit =
  val head = ptr - 8
  val size = getSize(head)
  val selected = selectBucket(size)
  appendBlock(selected, head)

def debugShowBucket(start: Ptr, name: String): Unit =
  def go(cur: Ptr): Unit =
    if cur < 0 then ()
    else
      putStr("* block at address ")
      #i32println(cur)
      putStr("  size = ")
      #i32println(getSize(cur))
      go(getNext(cur))
  putStr("> Start of Bucket List ")
  putStrLn(name)
  go(WASM_MEMORY(start))
  putStr("< End of Bucket List ")
  putStrLn(name)

def selectBucket(size: i32): Ptr = getBucketHead(size)

struct Ref(var data: i32)
def testRound(): i32 =
  val TEST_SIZE = 500
  val ptrs = newArray(TEST_SIZE, 0)
  val numSuccess = Ref(0)
  (0.until(TEST_SIZE)).iterate: i =>
    val t = alloc(i)
    ptrs(i) = t
    if t > 0 then
      numSuccess.data = numSuccess.data + 1
    // putStr("The result of allocating a block of size ")
    // #i32println(i * 7)
    // putStr("  == ")
    // #i32println(t)
  (0.until(TEST_SIZE)).iterate: i =>
    if ptrs(i) > 0 then
      free(ptrs(i))

  numSuccess.data

def main(): Unit =
  (1.until(1000)).iterate: idx =>
    val elapsed = benchmark: () =>
      val numSuccess = testRound()
      putStr("  > success allocs = ")
      #i32println(numSuccess)
    putStr("Time of round ")
    #i32println(idx)
    putStr("  =  ")
    #i32println(elapsed)
  

