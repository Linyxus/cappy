final class MarkerRunnable extends java.lang.Runnable:
  var ran = false

  def run(): Unit =
    ran = true

@main def markersRunnable(): Unit =
  val runnable = new MarkerRunnable
  runnable.run()
  println("runnable:" + runnable.ran)
