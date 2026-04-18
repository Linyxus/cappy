import java.io.{EOFException, IOException}
import java.lang.{
  IllegalArgumentException,
  NullPointerException,
  RuntimeException,
  Throwable
}

class MyErr(m: String) extends RuntimeException(m)

@main def javalibThrowablesCatch(): Unit =
  // 1. Throw and catch the exact type; bind the value through a try/catch
  //    in expression position.
  val exactMsg: String =
    try
      throw new IllegalArgumentException("bad-arg")
    catch
      case e: IllegalArgumentException => e.getMessage()
  println("exact:" + exactMsg)

  // 2. Throw a subclass (EOFException), catch as supertype (IOException).
  val subclassMsg: String =
    try
      throw new EOFException("eof")
    catch
      case e: IOException => e.getMessage()
  println("subclass:" + subclassMsg)

  // 3. Throw, catch as `Throwable` (root of the ported hierarchy).
  val rootMsg: String =
    try
      throw new RuntimeException("rt")
    catch
      case e: Throwable => e.getMessage()
  println("as-throwable:" + rootMsg)

  // 4. Non-matching first case falls through to a matching second case; the
  //    un-matched arm must NOT swallow the exception.
  val fallthroughMsg: String =
    try
      throw new IllegalArgumentException("picked-by-second")
    catch
      case _: NullPointerException         => "wrong-first"
      case e: IllegalArgumentException     => "picked:" + e.getMessage()
  println("fallthrough:" + fallthroughMsg)

  // 5. Cause chain survives a throw/catch round-trip.
  val causeMsg: String =
    try
      throw new RuntimeException("outer", new IllegalArgumentException("inner"))
    catch
      case e: RuntimeException =>
        val c = e.getCause().asInstanceOf[Throwable]
        e.getMessage() + "/" + c.getMessage()
  println("cause:" + causeMsg)

  // 6. User-defined Scala class extending a ported Throwable. Catch as
  //    the ported supertype and read back the dynamic class name +
  //    message.
  val customMsg: String =
    try
      throw new MyErr("boom")
    catch
      case e: RuntimeException =>
        e.getClass().getName() + ":" + e.getMessage()
  println("custom:" + customMsg)

  // 7. `finally` runs on the propagation path; the outer catch binds the
  //    exception after the inner `finally` has executed.
  val finallyResult: String =
    try
      try
        throw new RuntimeException("propagates")
      finally
        println("finally:inner-finally-ran")
    catch
      case e: RuntimeException => e.getMessage()
  println("finally:" + finallyResult)

  // 8. `try ... finally` in expression position produces the block's
  //    value; `finally` runs regardless and is observable via a side
  //    effect on a `var`.
  var finallyCounter = 0
  val finallyValue: Int =
    try 42
    finally finallyCounter += 1
  println("finally-value:" + finallyValue + ":" + finallyCounter)

  // 9. `try/catch/finally` combined in expression position.
  val caughtAndFinalized: Int =
    try throw new RuntimeException("swallowed")
    catch case _: RuntimeException => 99
    finally finallyCounter += 1
  println("catch+finally:" + caughtAndFinalized + ":" + finallyCounter)
