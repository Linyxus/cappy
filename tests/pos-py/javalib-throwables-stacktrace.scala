import java.lang.{RuntimeException, StackTraceElement, Throwable}

private def leafThrowable(): Throwable =
  new RuntimeException("boom")

private def middleThrowable(): Throwable =
  leafThrowable()

private def topThrowable(): Throwable =
  middleThrowable()

@main def javalibThrowablesStacktrace(): Unit =
  val throwable = topThrowable()
  val stack = throwable.getStackTrace()

  println("auto-size:" + stack.length)
  println("auto-top:" + stack(0).getMethodName() + ":" + stack(0).getFileName())
  println("auto-next:" + stack(1).getMethodName() + ":" + stack(1).getFileName())
  println("auto-third:" + stack(2).getMethodName() + ":" + stack(2).getFileName())

  val manual = new StackTraceElement("demo.Owner", "manual", "Demo.scala", 42)
  throwable.setStackTrace(Array(manual))
  println("manual:" + throwable.getStackTrace()(0))

  throwable.fillInStackTrace()
  val refreshed = throwable.getStackTrace()
  println("refill-top:" + refreshed(0).getMethodName() + ":" + refreshed(0).getFileName())
