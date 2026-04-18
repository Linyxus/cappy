import java.io.{EOFException, IOException, UnsupportedEncodingException}
import java.lang.{
  ArrayIndexOutOfBoundsException,
  AssertionError,
  ExceptionInInitializerError,
  IllegalArgumentException,
  IndexOutOfBoundsException,
  RuntimeException,
  Throwable
}

@main def javalibThrowablesChain(): Unit =
  val root = new IllegalArgumentException("root")
  val mid = new RuntimeException("mid", root)
  val top = new IOException("top", mid)
  top.addSuppressed(new EOFException("eof"))
  top.addSuppressed(new UnsupportedEncodingException("utf"))

  val blank = new Throwable("blank")
  blank.initCause(root)

  val copiedSuppressed = top.getSuppressed()
  copiedSuppressed(0) = root

  val topCause = top.getCause().asInstanceOf[Throwable]
  val rootCause = topCause.getCause().asInstanceOf[Throwable]
  val assertion = new AssertionError(root)
  val initError = new ExceptionInInitializerError(mid)

  println("chain:" + top.getMessage() + ":" + topCause.getMessage() + ":" + rootCause.getMessage())
  println(
    "suppressed:" +
      top.getSuppressed().length + ":" +
      top.getSuppressed()(0).getMessage() + ":" +
      top.getSuppressed()(1).getMessage()
  )
  println("suppressed-copy:" + top.getSuppressed()(0).getMessage())
  println("init-cause:" + blank.getCause().asInstanceOf[Throwable].getMessage())
  println("assertion-cause:" + (assertion.getCause() eq root))
  println("initializer:" + (initError.getCause() eq mid) + ":" + (initError.getMessage() == null))
  println(
    "index-messages:" +
      new IndexOutOfBoundsException(7).getMessage() + ":" +
      new ArrayIndexOutOfBoundsException(9).getMessage()
  )
