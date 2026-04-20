@main def javalibNioFloatBuffer(): Unit =
  val allocated = java.nio.FloatBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + (allocated.order() == java.nio.ByteOrder.nativeOrder()))

  val putGet = java.nio.FloatBuffer.allocate(3)
  putGet.put(1.5f).put(-2.25f).put(3.75f)
  putGet.flip()
  println(
    "put-get:" +
      (java.lang.Float.compare(putGet.get(), 1.5f) == 0) + ":" +
      (java.lang.Float.compare(putGet.get(), -2.25f) == 0) + ":" +
      putGet.remaining()
  )

  val flipBuf = java.nio.FloatBuffer.wrap(Array[Float](1.0f, 2.0f, 3.0f))
  flipBuf.position(2)
  flipBuf.flip()
  println("flip:" + flipBuf.limit() + ":" + flipBuf.position() + ":" + (java.lang.Float.compare(flipBuf.get(), 1.0f) == 0))

  val base = java.nio.FloatBuffer.wrap(Array[Float](7.0f, 8.0f, 9.0f, 10.0f))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 42.5f)
  println("slice:" + slice.capacity() + ":" + (java.lang.Float.compare(slice.get(0), 42.5f) == 0) + ":" + (java.lang.Float.compare(base.get(1), 42.5f) == 0))
