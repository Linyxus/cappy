import scala.python.runtime.PyThreading

@main def javalibThreadStartJoin(): Unit =
  val gate = PyThreading.newEvent()
  var counter = 0

  val worker = new java.lang.Thread(
    () =>
      gate.waitReady()
      counter = 41
    ,
    "worker-start"
  )

  println("alive-before:" + worker.isAlive())
  worker.start()
  worker.join(10L)
  println("alive-during:" + worker.isAlive())
  gate.set()
  worker.join()
  println("alive-after:" + worker.isAlive())
  println("result:" + counter + ":" + worker.getName() + ":" + worker.isDaemon())

  try
    worker.start()
    println("restart:false")
  catch
    case _: java.lang.IllegalThreadStateException =>
      println("restart:true")
