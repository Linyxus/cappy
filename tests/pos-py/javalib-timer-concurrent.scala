import java.util.{Timer, TimerTask}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import scala.python.runtime.PyThreading

private final class LatchTask(counter: AtomicInteger, done: CountDownLatch) extends TimerTask:
  def run(): Unit =
    counter.incrementAndGet()
    done.countDown()

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibTimerConcurrent(): Unit =
  val timer = new Timer()
  val start = PyThreading.newEvent()
  val done = new CountDownLatch(400)
  val counter = new AtomicInteger(0)

  val threads = Array(
    new java.lang.Thread(() =>
      start.waitReady()
      var i = 0
      while i < 100 do
        timer.schedule(new LatchTask(counter, done), 0L)
        i += 1
    ),
    new java.lang.Thread(() =>
      start.waitReady()
      var i = 0
      while i < 100 do
        timer.schedule(new LatchTask(counter, done), 0L)
        i += 1
    ),
    new java.lang.Thread(() =>
      start.waitReady()
      var i = 0
      while i < 100 do
        timer.schedule(new LatchTask(counter, done), 0L)
        i += 1
    ),
    new java.lang.Thread(() =>
      start.waitReady()
      var i = 0
      while i < 100 do
        timer.schedule(new LatchTask(counter, done), 0L)
        i += 1
    )
  )

  var index = 0
  while index < threads.length do
    threads(index).start()
    index += 1
  start.set()
  joinAll(threads)
  done.await()
  timer.cancel()
  println("timer-concurrent:" + counter.get() + ":" + done.getCount())
