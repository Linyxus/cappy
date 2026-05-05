// Exercises the JDK-shaped auxiliary constructors that pylib's
// `IOException` and `TimeoutException` expose. Stdlib code (and
// user fixtures like `tests/run/saferExceptions.scala` and
// `tests/run/tryPatternMatch.scala`) emits `<init>()`, `<init>(String)`,
// `<init>(Throwable)`, `<init>(String, Throwable)` references that
// the default-arg primary alone doesn't produce as distinct PyIR
// signatures.

import java.io.IOException
import java.util.concurrent.TimeoutException

@main def throwablesJdkCtors(): Unit =
  val cause = new RuntimeException("root")

  // IOException — all 4 JDK shapes.
  println(catching(throw new IOException))                            // <iohelp>
  println(catching(throw new IOException("msg")))                     // msg
  println(catching(throw new IOException("msg", cause)))              // msg
  println(catching(throw new IOException(cause)))                     // java.lang.RuntimeException: root

  // TimeoutException — `()` and `(String)`.
  println(catching(throw new TimeoutException))                       // <iohelp>
  println(catching(throw new TimeoutException("late")))               // late

def catching(body: => Unit): String =
  try
    body
    "<no-throw>"
  catch
    case t: Throwable =>
      val m = t.getMessage()
      if m == null then "<iohelp>" else m
