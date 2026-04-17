final class MarkerAutoCloseable extends java.lang.AutoCloseable:
  var closed = false

  def close(): Unit =
    closed = true

@main def markersAutoCloseable(): Unit =
  val value = new MarkerAutoCloseable
  value.close()
  println("auto-closeable:" + value.closed)
