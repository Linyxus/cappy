@main def javalibIoSystemStreams(): Unit =
  // System.out is a real java.io.PrintStream routed through
  // sys.stdout.buffer — exercise the byte-level and text-level API.
  // The test harness merges stderr into stdout, so any System.err
  // writes appear in the captured output too (that still pins the
  // routing — both sinks produce bytes we can diff).

  // write(Array[Byte]) — exercises the FilterOutputStream path.
  System.out.write(Array[Byte](0x73, 0x79, 0x73, 0x6f, 0x75, 0x74, 0x2d, 0x62, 0x79, 0x74, 0x65, 0x73, 0x3a))
  System.out.write('A'.toInt)
  System.out.write('\n'.toInt)

  // print / println variants.
  System.out.print("sysout-text:")
  System.out.println("hello")

  // printf / format funnels to the Formatter then to the encoder.
  System.out.printf("sysout-fmt:%d-%s%n", Array[Object](Integer.valueOf(42), "done"))

  // checkError() returns false in the common case; pin it.
  val beforeError = System.out.checkError()
  System.out.println("sysout-checkerror:" + beforeError)

  // System.err is a distinct PrintStream. Write to it and flush so
  // the bytes reach the captured stream before the setOut/setErr
  // swap below.
  System.err.println("syserr-text:hi")
  System.err.flush()

  // setOut round-trip: swap a capture buffer in, verify writes land
  // there, restore.
  val captured = new java.io.ByteArrayOutputStream()
  val capturePs = new java.io.PrintStream(captured, true)
  val savedOut = System.out
  System.setOut(capturePs)
  System.out.print("captured")
  System.setOut(savedOut)
  System.out.println("setout:" + new String(captured.toByteArray(), "UTF-8"))
