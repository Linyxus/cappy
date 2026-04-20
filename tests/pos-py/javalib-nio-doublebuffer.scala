@main def javalibNioDoubleBuffer(): Unit =
  val allocated = java.nio.DoubleBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + (allocated.order() == java.nio.ByteOrder.nativeOrder()))

  val putGet = java.nio.DoubleBuffer.allocate(3)
  putGet.put(1.5).put(-2.25).put(3.75)
  putGet.flip()
  println(
    "put-get:" +
      (java.lang.Double.compare(putGet.get(), 1.5) == 0) + ":" +
      (java.lang.Double.compare(putGet.get(), -2.25) == 0) + ":" +
      putGet.remaining()
  )

  val flipBuf = java.nio.DoubleBuffer.wrap(Array[Double](1.0, 2.0, 3.0))
  flipBuf.position(2)
  flipBuf.flip()
  println("flip:" + flipBuf.limit() + ":" + flipBuf.position() + ":" + (java.lang.Double.compare(flipBuf.get(), 1.0) == 0))

  val base = java.nio.DoubleBuffer.wrap(Array[Double](7.0, 8.0, 9.0, 10.0))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 42.5)
  println("slice:" + slice.capacity() + ":" + (java.lang.Double.compare(slice.get(0), 42.5) == 0) + ":" + (java.lang.Double.compare(base.get(1), 42.5) == 0))
