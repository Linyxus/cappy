@main def javalibNioShortBuffer(): Unit =
  val allocated = java.nio.ShortBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + (allocated.order() == java.nio.ByteOrder.nativeOrder()))

  val putGet = java.nio.ShortBuffer.allocate(3)
  putGet.put(10.toShort).put((-2).toShort).put(30.toShort)
  putGet.flip()
  println("put-get:" + putGet.get() + ":" + putGet.get() + ":" + putGet.remaining())

  val flipBuf = java.nio.ShortBuffer.wrap(Array[Short](1.toShort, 2.toShort, 3.toShort))
  flipBuf.position(2)
  flipBuf.flip()
  println("flip:" + flipBuf.limit() + ":" + flipBuf.position() + ":" + flipBuf.get())

  val base = java.nio.ShortBuffer.wrap(Array[Short](7.toShort, 8.toShort, 9.toShort, 10.toShort))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 42.toShort)
  println("slice:" + slice.capacity() + ":" + slice.get(0) + ":" + base.get(1))
