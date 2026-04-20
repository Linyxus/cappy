@main def javalibIoBufferedReader(): Unit =
  val lf = new java.io.BufferedReader(new java.io.StringReader("a\nb\n"))
  println("readline:lf:" + String.valueOf(lf.readLine()) + ":" + String.valueOf(lf.readLine()) + ":" + String.valueOf(lf.readLine()))

  val crlf = new java.io.BufferedReader(new java.io.StringReader("a\r\nb\r\n"))
  println("readline:crlf:" + String.valueOf(crlf.readLine()) + ":" + String.valueOf(crlf.readLine()) + ":" + String.valueOf(crlf.readLine()))

  val eofMid = new java.io.BufferedReader(new java.io.StringReader("tail"))
  println("readline:eof-mid:" + String.valueOf(eofMid.readLine()) + ":" + String.valueOf(eofMid.readLine()))

  val empty = new java.io.BufferedReader(new java.io.StringReader("\n\nx\n"))
  println("readline:empty:" + String.valueOf(empty.readLine()) + ":" + String.valueOf(empty.readLine()) + ":" + String.valueOf(empty.readLine()) + ":" + String.valueOf(empty.readLine()))

  val marked = new java.io.BufferedReader(new java.io.StringReader("abc\ndef"))
  val prefix = new Array[Char](2)
  marked.read(prefix)
  marked.mark(8)
  val next0 = marked.read().toChar
  marked.reset()
  val next1 = marked.read().toChar
  println("readline:mark:" + new String(prefix) + ":" + next0 + ":" + next1)
