private def renderBytes(xs: Array[Byte]): String =
  var out = ""
  var i = 0
  while i < xs.length do
    if i != 0 then out += ","
    out += (xs(i) & 0xff).toString
    i += 1
  out

@main def javalibNioByteBuffer(): Unit =
  val allocated = java.nio.ByteBuffer.allocate(4)
  println("allocate:" + allocated.capacity() + ":" + allocated.hasArray() + ":" + allocated.isDirect())

  val direct = java.nio.ByteBuffer.allocateDirect(2)
  println("allocate-direct:" + direct.capacity() + ":" + direct.hasArray() + ":" + direct.isDirect())

  val putGet = java.nio.ByteBuffer.allocate(3)
  putGet.put(1.toByte).put(2.toByte).put(3.toByte)
  putGet.flip()
  println("put-get:" + putGet.get() + ":" + putGet.get() + ":" + putGet.get())

  val flipBuf = java.nio.ByteBuffer.allocate(4)
  flipBuf.put(Array[Byte](7, 8))
  flipBuf.flip()
  println("flip:" + flipBuf.remaining() + ":" + flipBuf.get() + ":" + flipBuf.remaining())

  val sliceBase = java.nio.ByteBuffer.wrap(Array[Byte](10, 20, 30, 40))
  sliceBase.position(1)
  sliceBase.limit(3)
  val slice = sliceBase.slice()
  slice.put(0, 99.toByte)
  println("slice:" + slice.capacity() + ":" + slice.get(0) + ":" + sliceBase.get(1) + ":" + renderBytes(sliceBase.array()))

  val viewBase = java.nio.ByteBuffer.allocate(8).order(java.nio.ByteOrder.LITTLE_ENDIAN)
  val intView = viewBase.asIntBuffer()
  intView.put(0, 0x01020304)
  println("view:" + renderBytes(viewBase.array()) + ":" + intView.get(0))
