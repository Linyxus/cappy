package java.util.concurrent

import java.lang.Thread

import scala.python.runtime.PyThreading

class CountDownLatch(count: Int) extends java.io.Serializable:
  import CountDownLatch.*

  if count < 0 then
    throw new IllegalArgumentException()

  private val lock = PyThreading.newRLock()
  private val condition = PyThreading.newCondition(lock)
  private var count0 = count.toLong

  def await(): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    lock.acquire()
    try
      while count0 > 0L do
        condition.waitReady(PollMillis)
        if Thread.interrupted() then
          throw new InterruptedException(null)
    finally lock.release()

  def await(timeout: Long, unit: TimeUnit): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    var remaining = timeoutToMillis(timeout, unit)
    lock.acquire()
    try
      while count0 > 0L do
        if remaining <= 0L then
          return false
        val slice =
          if remaining < PollMillis then remaining
          else PollMillis
        condition.waitReady(slice)
        remaining -= slice
        if Thread.interrupted() then
          throw new InterruptedException(null)
      true
    finally lock.release()

  def countDown(): Unit =
    lock.acquire()
    try
      if count0 > 0L then
        count0 -= 1L
        if count0 == 0L then
          condition.notifyAllThreads()
    finally lock.release()

  def getCount(): Long =
    lock.acquire()
    try count0
    finally lock.release()

  override def toString(): String =
    s"${super.toString}[Count = ${getCount()}]"

object CountDownLatch:
  private final val PollMillis = 5L

  private def timeoutToMillis(timeout: Long, unit: TimeUnit): Long =
    if timeout <= 0L then 0L
    else
      val millis = unit.toMillis(timeout)
      if millis > 0L then millis else 1L
