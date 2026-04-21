import scala.python.runtime.PyThreading

@main def javalibThreadInterrupt(): Unit =
  val ready = PyThreading.newEvent()
  var caught = "missed"
  var cleared = false

  val worker = new java.lang.Thread(
    () =>
      try
        ready.set()
        java.lang.Thread.sleep(1000L)
      catch
        case _: java.lang.InterruptedException =>
          val current = java.lang.Thread.currentThread()
          caught =
            "caught:" + current.isInterrupted() + ":" +
              java.lang.Thread.interrupted() + ":" +
              current.isInterrupted()
          cleared = !current.isInterrupted()
    ,
    "worker-interrupt"
  )

  worker.start()
  ready.waitReady()
  worker.interrupt()
  worker.join()
  println(caught)
  println("target-cleared:" + cleared + ":" + worker.isInterrupted())

  val current = java.lang.Thread.currentThread()
  current.interrupt()
  println(
    "read-vs-clear:" +
      current.isInterrupted() + ":" +
      current.isInterrupted() + ":" +
      java.lang.Thread.interrupted() + ":" +
      java.lang.Thread.interrupted()
  )
