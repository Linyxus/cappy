final class MarkerFlushable extends java.io.Flushable:
  var flushed = false

  def flush(): Unit =
    flushed = true

@main def markersFlushable(): Unit =
  val flushable = new MarkerFlushable
  flushable.flush()
  println("flushable:" + flushable.flushed)
