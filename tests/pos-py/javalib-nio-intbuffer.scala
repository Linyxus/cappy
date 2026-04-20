@main def javalibNioIntBuffer(): Unit =
  val allocated = java.nio.IntBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + (allocated.order() == java.nio.ByteOrder.nativeOrder()))

  val putGet = java.nio.IntBuffer.allocate(3)
  putGet.put(10).put(20).put(30)
  putGet.flip()
  println("put-get:" + putGet.get() + ":" + putGet.get() + ":" + putGet.remaining())

  val flipBuf = java.nio.IntBuffer.wrap(Array(1, 2, 3))
  flipBuf.position(2)
  flipBuf.flip()
  println("flip:" + flipBuf.limit() + ":" + flipBuf.position() + ":" + flipBuf.get())

  val base = java.nio.IntBuffer.wrap(Array(7, 8, 9, 10))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 42)
  println("slice:" + slice.capacity() + ":" + slice.get(0) + ":" + base.get(1))
