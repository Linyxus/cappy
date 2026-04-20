private def renderBytes(xs: Array[Byte]): String =
  var out = ""
  var i = 0
  while i < xs.length do
    if i != 0 then out += ","
    out += (xs(i) & 0xff).toString
    i += 1
  out

@main def javalibNioByteOrder(): Unit =
  println("byteorder:native:" + java.nio.ByteOrder.nativeOrder())

  val big = java.nio.ByteBuffer.allocate(4)
  big.putInt(0x01020304)
  println("byteorder:big:" + renderBytes(big.array()))

  val little = java.nio.ByteBuffer.allocate(4).order(java.nio.ByteOrder.LITTLE_ENDIAN)
  little.putInt(0x01020304)
  println("byteorder:little:" + renderBytes(little.array()))
