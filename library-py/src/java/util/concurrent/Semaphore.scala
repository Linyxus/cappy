package java.util.concurrent

import java.lang.Thread
import java.util.{Collection, Collections}

import scala.python.runtime.PyThreading

class Semaphore(private[this] var permits: Int, fairness: Boolean) extends java.io.Serializable:
  import Semaphore.*

  private val lock = PyThreading.newRLock()
  private val condition = PyThreading.newCondition(lock)

  def this(permits: Int) =
    this(permits, false)

  def acquire(): Unit =
    acquire(1)

  def acquire(permits: Int): Unit =
    requireNonNegative(permits)
    if Thread.interrupted() then
      throw new InterruptedException(null)
    lock.acquire()
    try
      while this.permits < permits do
        condition.waitReady(PollMillis)
        if Thread.interrupted() then
          throw new InterruptedException(null)
      this.permits -= permits
    finally lock.release()

  def acquireUninterruptibly(): Unit =
    acquireUninterruptibly(1)

  def acquireUninterruptibly(permits: Int): Unit =
    requireNonNegative(permits)
    var interrupted = false
    lock.acquire()
    try
      while this.permits < permits do
        condition.waitReady(PollMillis)
        if Thread.interrupted() then
          interrupted = true
      this.permits -= permits
    finally lock.release()
    if interrupted then
      Thread.currentThread().interrupt()

  def availablePermits(): Int =
    lock.acquire()
    try permits
    finally lock.release()

  def drainPermits(): Int =
    lock.acquire()
    try
      val old = permits
      permits = 0
      old
    finally lock.release()

  protected def getQueuedThreads(): Collection[Thread] =
    Collections.emptySet()

  final def getQueueLength(): Int =
    0

  final def hasQueuedThreads(): Boolean =
    false

  def isFair(): Boolean =
    fairness

  protected def reducePermits(reduction: Int): Unit =
    requireNonNegative(reduction)
    lock.acquire()
    try permits -= reduction
    finally lock.release()

  def release(): Unit =
    release(1)

  def release(permits: Int): Unit =
    requireNonNegative(permits)
    lock.acquire()
    try
      this.permits += permits
      if permits > 0 then
        condition.notifyAllThreads()
    finally lock.release()

  override def toString(): String =
    s"${super.toString}[Permits = ${availablePermits()}]"

  def tryAcquire(): Boolean =
    tryAcquire(1)

  def tryAcquire(permits: Int): Boolean =
    requireNonNegative(permits)
    lock.acquire()
    try
      if this.permits >= permits then
        this.permits -= permits
        true
      else
        false
    finally lock.release()

  def tryAcquire(timeout: Long, unit: TimeUnit): Boolean =
    tryAcquire(1, timeout, unit)

  def tryAcquire(permits: Int, timeout: Long, unit: TimeUnit): Boolean =
    requireNonNegative(permits)
    if Thread.interrupted() then
      throw new InterruptedException(null)
    var remaining = timeoutToMillis(timeout, unit)
    lock.acquire()
    try
      while this.permits < permits do
        if remaining <= 0L then
          return false
        val slice =
          if remaining < PollMillis then remaining
          else PollMillis
        condition.waitReady(slice)
        remaining -= slice
        if Thread.interrupted() then
          throw new InterruptedException(null)
      this.permits -= permits
      true
    finally lock.release()

object Semaphore:
  private final val PollMillis = 5L

  private def requireNonNegative(n: Int): Unit =
    if n < 0 then
      throw new IllegalArgumentException()

  private def timeoutToMillis(timeout: Long, unit: TimeUnit): Long =
    if timeout <= 0L then 0L
    else
      val millis = unit.toMillis(timeout)
      if millis > 0L then millis else 1L
