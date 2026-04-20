@main def javalibIoCharStreams(): Unit =
  val readerTarget = java.nio.CharBuffer.allocate(5)
  val readerCount = new java.io.StringReader("scala").read(readerTarget)
  readerTarget.flip()
  println("reader:" + readerCount + ":" + readerTarget.toString())

  val sr = new java.io.StringReader("hello")
  val srBuf = new Array[Char](3)
  val srCount = sr.read(srBuf)
  sr.mark(8)
  val srNext = sr.read()
  sr.reset()
  val srAgain = sr.read()
  println("stringreader:" + srCount + ":" + new String(srBuf) + ":" + srNext + ":" + srAgain + ":" + sr.skip(1))

  val sw = new java.io.StringWriter()
  sw.write("ab")
  sw.append('c')
  sw.write("def", 1, 2)
  println("stringwriter:" + sw.toString())

  val car = new java.io.CharArrayReader("xyz".toCharArray(), 1, 2)
  val carBuf = new Array[Char](2)
  val carCount = car.read(carBuf)
  println("chararrayreader:" + carCount + ":" + new String(carBuf) + ":" + car.ready())

  val caw = new java.io.CharArrayWriter()
  caw.write("abc".toCharArray(), 1, 2)
  caw.write("wxyz", 1, 2)
  caw.write("!")
  println("chararraywriter:" + caw.size() + ":" + caw.toString())

  val printSink = new java.io.StringWriter()
  val pw = new java.io.PrintWriter(printSink, true)
  pw.print("hi")
  pw.println(" there")
  pw.write("!")
  pw.flush()
  println("printwriter:" + printSink.toString().replace("\n", "\\n") + ":" + pw.checkError())

  val bytesOut = new java.io.ByteArrayOutputStream()
  val osw = new java.io.OutputStreamWriter(bytesOut, "UTF-8")
  val text = "hi " + Character.toString(0x1F600)
  osw.write(text)
  osw.close()
  println("outputstreamwriter:" + (new String(bytesOut.toByteArray(), "UTF-8") == text))

  val isr = new java.io.InputStreamReader(new java.io.ByteArrayInputStream(bytesOut.toByteArray()), "UTF-8")
  val isrBuf = new Array[Char](5)
  val isrCount = isr.read(isrBuf, 0, isrBuf.length)
  println("inputstreamreader:" + isrCount + ":" + (new String(isrBuf, 0, isrCount) == text))

  val latinOut = new java.io.ByteArrayOutputStream()
  val latinWriter = new java.io.OutputStreamWriter(latinOut, "ISO-8859-1")
  val latinEncoding = latinWriter.getEncoding()
  latinWriter.write("hé")
  latinWriter.close()
  println(
    "latin1-roundtrip:" +
      (new String(latinOut.toByteArray(), "ISO-8859-1") == "hé") +
      ":" +
      latinEncoding
  )

  val latinReader = new java.io.InputStreamReader(
    new java.io.ByteArrayInputStream(Array[Byte](104.toByte, -23.toByte)),
    "ISO-8859-1"
  )
  val latinBuf = new Array[Char](8)
  val latinCount = latinReader.read(latinBuf, 0, latinBuf.length)
  println(
    "latin1-reader:" +
      latinCount +
      ":" +
      new String(latinBuf, 0, latinCount) +
      ":" +
      latinReader.getEncoding()
  )

  val bogusCharset =
    try
      new java.io.OutputStreamWriter(new java.io.ByteArrayOutputStream(), "totally-fake")
      false
    catch
      case _: java.io.UnsupportedEncodingException => true
  println("bogus-charset:" + bogusCharset)
