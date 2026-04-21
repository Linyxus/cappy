import java.util.concurrent.TimeUnit

@main def javalibConcurrentTimeUnit(): Unit =
  val millisFromSeconds = TimeUnit.MILLISECONDS.convert(2L, TimeUnit.SECONDS)
  val secondsFromMillis = TimeUnit.SECONDS.convert(2500L, TimeUnit.MILLISECONDS)
  val nanosFromMillis = TimeUnit.MILLISECONDS.toNanos(3L)
  println("convert:" + millisFromSeconds + ":" + secondsFromMillis + ":" + nanosFromMillis)
