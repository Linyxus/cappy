import java.io.{EOFException, IOException}
import java.lang.{
  IllegalArgumentException,
  NullPointerException,
  RuntimeException,
  Throwable
}

class MyErr(m: String) extends RuntimeException(m)

// NB: every scenario below uses `try/catch` in *statement* position,
// writing into a `var` declared outside the try. The compiler currently
// emits the `case _ => PyUnitLit()` fallback in `genExpr` for
// `try/catch`-as-expression, silently erasing the whole construct
// (see H5 audit report — separate bug, deliberately NOT papered over
// here). Keeping the uses statement-shaped exercises the raise/except
// path cleanly.
@main def javalibThrowablesCatch(): Unit =
  // 1. Throw and catch the exact type; read the message back off the binding.
  var exactMsg: String = ""
  try
    throw new IllegalArgumentException("bad-arg")
  catch
    case e: IllegalArgumentException =>
      exactMsg = e.getMessage()
  println("exact:" + exactMsg)

  // 2. Throw a subclass (EOFException), catch as supertype (IOException).
  var subclassMsg: String = ""
  try
    throw new EOFException("eof")
  catch
    case e: IOException =>
      subclassMsg = e.getMessage()
  println("subclass:" + subclassMsg)

  // 3. Throw, catch as `Throwable` (root of the ported hierarchy).
  var rootMsg: String = ""
  try
    throw new RuntimeException("rt")
  catch
    case e: Throwable =>
      rootMsg = e.getMessage()
  println("as-throwable:" + rootMsg)

  // 4. Non-matching first case falls through to a matching second case; the
  //    un-matched arm must NOT swallow the exception. First arm catches
  //    NullPointerException (won't match); second matches.
  var fallthroughMsg: String = ""
  try
    throw new IllegalArgumentException("picked-by-second")
  catch
    case _: NullPointerException =>
      fallthroughMsg = "wrong-first"
    case e: IllegalArgumentException =>
      fallthroughMsg = "picked:" + e.getMessage()
  println("fallthrough:" + fallthroughMsg)

  // 5. Cause chain survives a throw/catch round-trip.
  var causeMsg: String = ""
  try
    throw new RuntimeException("outer", new IllegalArgumentException("inner"))
  catch
    case e: RuntimeException =>
      val c = e.getCause().asInstanceOf[Throwable]
      causeMsg = e.getMessage() + "/" + c.getMessage()
  println("cause:" + causeMsg)

  // 6. User-defined Scala class extending a ported Throwable.
  //    Catch as the ported supertype (RuntimeException) and read both the
  //    dynamic class name and the message back off the binding.
  var customMsg: String = ""
  try
    throw new MyErr("boom")
  catch
    case e: RuntimeException =>
      customMsg = e.getClass().getName() + ":" + e.getMessage()
  println("custom:" + customMsg)

  // 7. `finally` runs when the exception propagates out of the inner try.
  //    Outer catch demonstrates the exception actually reaches the outer
  //    handler after `finally` has executed.
  var finallyResult: String = ""
  try
    try
      throw new RuntimeException("propagates")
    finally
      println("finally:inner-finally-ran")
  catch
    case e: RuntimeException =>
      finallyResult = e.getMessage()
  println("finally:" + finallyResult)
