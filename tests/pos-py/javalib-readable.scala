final class MarkerReadable extends java.lang.Readable:
  def read(cb: java.nio.CharBuffer): Int = 7

@main def markersReadable(): Unit =
  val cb = null.asInstanceOf[java.nio.CharBuffer]
  println("readable:" + (new MarkerReadable).read(cb))
