import scala.python.runtime.PyThreading

final class SyncLockBasic

@main def javalibSyncBasic(): Unit =
  val lock = new SyncLockBasic()
  var counter = 0
  val start = PyThreading.newEvent()

  def worker(): Unit =
    start.waitReady()
    var i = 0
    while i < 1000 do
      lock.synchronized {
        counter += 1
      }
      i += 1

  val t1 = PyThreading.newThread(() => worker())
  val t2 = PyThreading.newThread(() => worker())
  val t3 = PyThreading.newThread(() => worker())
  val t4 = PyThreading.newThread(() => worker())

  t1.start()
  t2.start()
  t3.start()
  t4.start()
  start.set()

  t1.join()
  t2.join()
  t3.join()
  t4.join()

  println("counter:" + counter)
