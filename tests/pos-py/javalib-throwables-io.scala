import java.io.{EOFException, IOException}
import java.lang.StackTraceElement

@main def javalibThrowablesIo(): Unit =
  val cause = new EOFException("disk-short-read")
  cause.setStackTrace(
    Array(
      new StackTraceElement("io.Decoder", "decode", "Decoder.scala", 8),
      new StackTraceElement("app.Service", "run", "Service.scala", 19),
      new StackTraceElement("app.Main", "main", "Main.scala", 5)
    )
  )

  val outer = new IOException("read failed", cause)
  outer.setStackTrace(
    Array(
      new StackTraceElement("io.Loader", "load", "Loader.scala", 21),
      new StackTraceElement("app.Service", "run", "Service.scala", 19),
      new StackTraceElement("app.Main", "main", "Main.scala", 5)
    )
  )

  // Outer throwable basic properties.
  println(
    "io:" +
      outer.getClass().getName() + ":" +
      outer.getMessage() + ":" +
      outer.getCause().asInstanceOf[Throwable].getClass().getName()
  )

  // `Throwable.toString` — the same format used by `printStackTrace`'s head line.
  println("to-string:" + outer.toString())
  println("cause-to-string:" + cause.toString())

  // Cause chain: getCause + getCause's class name.
  val actualCause = outer.getCause().asInstanceOf[Throwable]
  println("cause-class:" + actualCause.getClass().getName())
  println("cause-message:" + actualCause.getMessage())

  // Stack-trace arrays — verify setStackTrace recorded the frames on both sides.
  val outerFrames = outer.getStackTrace()
  println("outer-frames:" + outerFrames.length)
  println("outer-frame0:" + outerFrames(0).toString())
  println("outer-frame-last:" + outerFrames(outerFrames.length - 1).toString())

  val causeFrames = cause.getStackTrace()
  println("cause-frames:" + causeFrames.length)
  println("cause-frame0:" + causeFrames(0).toString())

  // Structural field access on a StackTraceElement.
  val f0 = outerFrames(0)
  println(
    "frame0-parts:" +
      f0.getClassName() + ":" +
      f0.getMethodName() + ":" +
      f0.getFileName() + ":" +
      f0.getLineNumber()
  )
