private def roundTripUtf(s: String): String =
  val out = new java.io.ByteArrayOutputStream()
  val dout = new java.io.DataOutputStream(out)
  dout.writeUTF(s)
  val din = new java.io.DataInputStream(new java.io.ByteArrayInputStream(out.toByteArray()))
  din.readUTF()

@main def javalibIoDataio(): Unit =
  println("readutf:ascii:" + (roundTripUtf("hello") == "hello"))
  println("readutf:two-byte:" + (roundTripUtf("caf\u00e9") == "caf\u00e9"))
  println("readutf:three-byte:" + (roundTripUtf("\u2603") == "\u2603"))
  println("readutf:surrogate:" + (roundTripUtf("\uD83D\uDE00") == "\uD83D\uDE00"))
  println("readutf:null:" + (roundTripUtf("A\u0000B") == "A\u0000B"))
