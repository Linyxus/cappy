import scala.python.runtime.PyThreading

@main def javalibThreadCurrentthread(): Unit =
  val ready = PyThreading.newEvent()
  val main = java.lang.Thread.currentThread()
  val mainId = main.getId()
  var workerLine = ""

  val worker = new java.lang.Thread(
    () =>
      val current = java.lang.Thread.currentThread()
      workerLine =
        "worker:" + (current eq java.lang.Thread.currentThread()) + ":" +
          (current ne main) + ":" +
          current.getName() + ":" +
          (current.getId() != mainId)
      ready.set()
    ,
    "worker-current"
  )

  println("main:" + (main eq java.lang.Thread.currentThread()) + ":" + main.getName() + ":" + (main.getId() == 1L))
  worker.start()
  ready.waitReady()
  worker.join()
  println(workerLine)
