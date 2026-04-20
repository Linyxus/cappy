@main def javalibNioLongBuffer(): Unit =
  val allocated = java.nio.LongBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + (allocated.order() == java.nio.ByteOrder.nativeOrder()))

  val putGet = java.nio.LongBuffer.allocate(3)
  putGet.put(10L).put(20L).put(30L)
  putGet.flip()
  println("put-get:" + putGet.get() + ":" + putGet.get() + ":" + putGet.remaining())

  val flipBuf = java.nio.LongBuffer.wrap(Array[Long](1L, 2L, 3L))
  flipBuf.position(2)
  flipBuf.flip()
  println("flip:" + flipBuf.limit() + ":" + flipBuf.position() + ":" + flipBuf.get())

  val base = java.nio.LongBuffer.wrap(Array[Long](7L, 8L, 9L, 10L))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 42L)
  println("slice:" + slice.capacity() + ":" + slice.get(0) + ":" + base.get(1))
