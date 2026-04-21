import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock

import scala.python.runtime.PyThreading

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibConcurrentLocks(): Unit =
  val reentrantLock = new ReentrantLock()
  reentrantLock.lock()
  reentrantLock.lock()
  val holdCount = reentrantLock.getHoldCount()
  val heldByCurrent = reentrantLock.isHeldByCurrentThread()
  reentrantLock.unlock()
  reentrantLock.unlock()

  val tryStart = PyThreading.newEvent()
  var workerTry = true
  reentrantLock.lock()
  val tryThread = new java.lang.Thread(() =>
    tryStart.waitReady()
    workerTry = reentrantLock.tryLock()
  )
  tryThread.start()
  tryStart.set()
  tryThread.join()
  reentrantLock.unlock()
  println("reentrantlock-try:" + holdCount + ":" + heldByCurrent + ":" + workerTry + ":" + reentrantLock.isLocked())

  val conditionLock = new ReentrantLock()
  val condition = conditionLock.newCondition()
  val waiting = PyThreading.newEvent()
  var released = false
  var awakened = false
  val conditionThread = new java.lang.Thread(() =>
    conditionLock.lock()
    try
      waiting.set()
      while !released do
        condition.await()
      awakened = true
    finally
      conditionLock.unlock()
  )
  conditionThread.start()
  waiting.waitReady()
  conditionLock.lock()
  try
    released = true
    condition.signalAll()
  finally
    conditionLock.unlock()
  conditionThread.join()
  println("condition-signal:" + awakened)

  val semaphore = new Semaphore(0)
  val acquiring = PyThreading.newEvent()
  var acquired = false
  val semaphoreThread = new java.lang.Thread(() =>
    acquiring.set()
    semaphore.acquire()
    acquired = true
  )
  semaphoreThread.start()
  acquiring.waitReady()
  val beforeRelease = acquired
  semaphore.release()
  semaphoreThread.join()
  println("semaphore-acquire:" + beforeRelease + ":" + acquired + ":" + semaphore.availablePermits())

  val latch = new CountDownLatch(2)
  var latchReleased = false
  val latchThread = new java.lang.Thread(() =>
    latch.await()
    latchReleased = true
  )
  latchThread.start()
  latch.countDown()
  val afterOne = latchReleased
  latch.countDown()
  latchThread.join()
  println("countdownlatch:" + afterOne + ":" + latchReleased + ":" + latch.getCount())

  val barrier = new CyclicBarrier(3)
  val barrierStart = PyThreading.newEvent()
  val indexSum = new AtomicInteger(0)
  val barrierThreads = Array(
    new java.lang.Thread(() =>
      barrierStart.waitReady()
      indexSum.addAndGet(barrier.await())
      ()
    ),
    new java.lang.Thread(() =>
      barrierStart.waitReady()
      indexSum.addAndGet(barrier.await())
      ()
    )
  )
  var barrierIndex = 0
  while barrierIndex < barrierThreads.length do
    barrierThreads(barrierIndex).start()
    barrierIndex += 1
  barrierStart.set()
  indexSum.addAndGet(barrier.await())
  joinAll(barrierThreads)
  println("cyclicbarrier:" + indexSum.get() + ":" + barrier.getParties() + ":" + barrier.getNumberWaiting())
