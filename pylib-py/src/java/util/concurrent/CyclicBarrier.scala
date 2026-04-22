package java.util.concurrent

import java.lang.Thread

import scala.python.runtime.PyThreading

class CyclicBarrier(parties: Int, barrierAction: Runnable | Null) extends java.io.Serializable:
  import CyclicBarrier.*

  if parties <= 0 then
    throw new IllegalArgumentException()

  private val lock = PyThreading.newRLock()
  private val condition = PyThreading.newCondition(lock)
  private var generation = 0L
  private var count = parties
  private var broken = false

  def this(parties: Int) =
    this(parties, null)

  def await(): Int =
    doWait(timed = false, 0L)

  def await(timeout: Long, unit: TimeUnit): Int =
    doWait(timed = true, timeoutToMillis(timeout, unit))

  def getNumberWaiting(): Int =
    lock.acquire()
    try parties - count
    finally lock.release()

  def getParties(): Int =
    parties

  def isBroken(): Boolean =
    lock.acquire()
    try broken
    finally lock.release()

  def reset(): Unit =
    lock.acquire()
    try
      breakBarrier()
      broken = false
    finally lock.release()

  private def doWait(timed: Boolean, timeoutMillis: Long): Int =
    lock.acquire()
    try
      if broken then
        throw new BrokenBarrierException()
      if Thread.interrupted() then
        breakBarrier()
        throw new InterruptedException(null)

      val myGeneration = generation
      count -= 1
      val arrivalIndex = count

      if count == 0 then
        try
          if barrierAction != null then
            barrierAction.run()
          nextGeneration()
          0
        catch
          case t: Throwable =>
            breakBarrier()
            throw t
      else
        var remaining = timeoutMillis
        while generation == myGeneration && !broken do
          if !timed then
            condition.waitReady(PollMillis)
          else
            if remaining <= 0L then
              breakBarrier()
              throw new TimeoutException()
            val slice =
              if remaining < PollMillis then remaining
              else PollMillis
            condition.waitReady(slice)
            remaining -= slice

          if generation == myGeneration && !broken && Thread.interrupted() then
            breakBarrier()
            throw new InterruptedException(null)

        if broken then
          throw new BrokenBarrierException()
        arrivalIndex
    finally lock.release()

  private def nextGeneration(): Unit =
    generation += 1L
    count = parties
    broken = false
    condition.notifyAllThreads()

  private def breakBarrier(): Unit =
    generation += 1L
    count = parties
    broken = true
    condition.notifyAllThreads()

object CyclicBarrier:
  private final val PollMillis = 5L

  private def timeoutToMillis(timeout: Long, unit: TimeUnit): Long =
    if timeout <= 0L then 0L
    else
      val millis = unit.toMillis(timeout)
      if millis > 0L then millis else 1L
