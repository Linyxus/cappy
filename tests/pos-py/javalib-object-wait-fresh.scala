import java.util.concurrent.CountDownLatch

final class FreshWaitLock

@main def javalibObjectWaitFresh(): Unit =
  val lock = new FreshWaitLock()
  val waiting = new CountDownLatch(1)
  val done = new CountDownLatch(1)
  var released = false
  var resumed = false

  val waiter = new java.lang.Thread(
    () =>
      lock.synchronized {
        waiting.countDown()
        while !released do
          lock.wait()
        resumed = true
      }
      done.countDown()
    ,
    "fresh-waiter"
  )

  waiter.start()
  waiting.await()
  lock.synchronized {
    released = true
    lock.notifyAll()
  }
  done.await()
  waiter.join()

  println("fresh-wait:" + resumed)
