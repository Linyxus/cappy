import scala.python.runtime.{PyMonitors, PyThreading}

final class WaitNotifyLock

final class Mailbox:
  var value = 0
  var ready = false

@main def javalibObjectWaitNotify(): Unit =
  val lock = new WaitNotifyLock()
  val monitor = PyMonitors.monitorFor(lock)
  val box = new Mailbox()
  var sum = 0

  val consumer = PyThreading.newThread(() =>
    var remaining = 3
    while remaining > 0 do
      monitor.acquire()
      try
        while !box.ready do
          lock.wait()
        sum += box.value
        box.ready = false
        lock.notifyAll()
        remaining -= 1
      finally
        monitor.release()
  )

  val producer = PyThreading.newThread(() =>
    var next = 1
    while next <= 3 do
      monitor.acquire()
      try
        while box.ready do
          lock.wait()
        box.value = next
        box.ready = true
        lock.notifyAll()
        next += 1
      finally
        monitor.release()
  )

  consumer.start()
  producer.start()
  consumer.join()
  producer.join()
  println("sum:" + sum)

  try
    lock.notifyAll()
    println("illegal-monitor:fail")
  catch
    case _: java.lang.IllegalMonitorStateException =>
      println("illegal-monitor:true")
