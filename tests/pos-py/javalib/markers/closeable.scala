final class MarkerCloseable extends java.io.Closeable:
  var closed = false

  def close(): Unit =
    closed = true

@main def markersCloseable(): Unit =
  val closeable = new MarkerCloseable
  closeable.close()
  println("closeable:" + closeable.closed)
