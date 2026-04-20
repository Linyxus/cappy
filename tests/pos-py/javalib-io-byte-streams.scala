private def utf8(xs: Array[Byte]): String =
  new String(xs, "UTF-8")

@main def javalibIoByteStreams(): Unit =
  val input = new java.io.ByteArrayInputStream("ABCD".getBytes("UTF-8"))
  val first = input.read()
  input.mark(8)
  val chunk = new Array[Byte](2)
  val chunkCount = input.read(chunk)
  input.reset()
  val afterReset = input.read()
  val skipped = input.skip(1)
  val last = input.read()
  println("inputstream:" + first + ":" + chunkCount + ":" + utf8(chunk) + ":" + afterReset + ":" + skipped + ":" + last + ":" + input.read())

  val nullOut = java.io.OutputStream.nullOutputStream()
  nullOut.write(Array[Byte](1, 2, 3))
  nullOut.close()
  val nullOutClosed =
    try
      nullOut.write(1)
      false
    catch
      case _: java.io.IOException => true
  println("outputstream:" + nullOutClosed)

  val pipedOut = new java.io.ByteArrayOutputStream()
  val transferred = new java.io.ByteArrayInputStream("pipe".getBytes("UTF-8")).transferTo(pipedOut)
  println("bytearray-pipe:" + transferred + ":" + utf8(pipedOut.toByteArray()))

  val dataBytes = new java.io.ByteArrayOutputStream()
  val dout = new java.io.DataOutputStream(dataBytes)
  dout.writeBoolean(true)
  dout.writeByte(255)
  dout.writeShort(0x1234)
  dout.writeChar(90)
  dout.writeInt(0x10203040)
  dout.writeLong(0x0102030405060708L)
  dout.writeFloat(1.5f)
  dout.writeDouble(-2.25)
  println("dataoutput:" + dout.size())

  val din = new java.io.DataInputStream(new java.io.ByteArrayInputStream(dataBytes.toByteArray()))
  println(
    "datainput:" +
      din.readBoolean() + ":" +
      din.readUnsignedByte() + ":" +
      din.readUnsignedShort() + ":" +
      din.readChar() + ":" +
      din.readInt() + ":" +
      din.readLong() + ":" +
      din.readFloat() + ":" +
      din.readDouble()
  )

  val psBytes = new java.io.ByteArrayOutputStream()
  val ps = new java.io.PrintStream(psBytes, true)
  ps.print("hello world")
  ps.println()
  ps.print("!!")
  ps.flush()
  println("printstream:" + utf8(psBytes.toByteArray()).replace("\n", "\\n") + ":" + ps.checkError())

  val printStreamGate =
    try
      new java.io.PrintStream(new java.io.ByteArrayOutputStream(), false, "ISO-8859-1")
      false
    catch
      case _: java.io.UnsupportedEncodingException => true
  println("printstream-gate:" + printStreamGate)
