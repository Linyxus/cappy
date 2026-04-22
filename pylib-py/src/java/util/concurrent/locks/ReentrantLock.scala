package java.util.concurrent.locks

import java.io.Serializable
import java.lang.{IllegalMonitorStateException, InterruptedException, System, Thread}
import java.util.Date
import java.util.concurrent.TimeUnit

import scala.python.runtime.{PyRLock, PyThreading}

class ReentrantLock(fair: Boolean) extends Lock with Serializable:
  import ReentrantLock.*

  private val pyLock = PyThreading.newRLock()
  private val metaLock = PyThreading.newLock()
  private var owner: Thread | Null = null
  private var holdCount0 = 0

  def this() =
    this(false)

  def lock(): Unit =
    pyLock.acquire()
    val current = Thread.currentThread()
    metaLock.acquire()
    try
      owner = current
      holdCount0 += 1
    finally metaLock.release()

  def lockInterruptibly(): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    while !pyLock.tryAcquire() do
      if Thread.interrupted() then
        throw new InterruptedException(null)
      PyThreading.sleep(PollMillis)

    val current = Thread.currentThread()
    metaLock.acquire()
    try
      owner = current
      holdCount0 += 1
    finally metaLock.release()

  def tryLock(): Boolean =
    if !pyLock.tryAcquire() then
      false
    else
      val current = Thread.currentThread()
      metaLock.acquire()
      try
        owner = current
        holdCount0 += 1
      finally metaLock.release()
      true

  def tryLock(time: Long, unit: TimeUnit): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)

    var remaining = timeoutToMillis(time, unit)
    while true do
      if pyLock.tryAcquire() then
        val current = Thread.currentThread()
        metaLock.acquire()
        try
          owner = current
          holdCount0 += 1
        finally metaLock.release()
        return true

      if remaining <= 0L then
        return false

      val slice =
        if remaining < PollMillis then remaining
        else PollMillis
      PyThreading.sleep(slice)
      remaining -= slice
      if Thread.interrupted() then
        throw new InterruptedException(null)

    false

  def unlock(): Unit =
    val current = Thread.currentThread()
    metaLock.acquire()
    try
      if owner.asInstanceOf[AnyRef] ne current.asInstanceOf[AnyRef] then
        throw new IllegalMonitorStateException()
      if holdCount0 <= 0 then
        throw new IllegalMonitorStateException()
      holdCount0 -= 1
      if holdCount0 == 0 then
        owner = null
    finally metaLock.release()
    pyLock.release()

  def newCondition(): Condition =
    new ReentrantCondition(this)

  def getHoldCount(): Int =
    metaLock.acquire()
    try holdCount0
    finally metaLock.release()

  def isHeldByCurrentThread(): Boolean =
    val current = Thread.currentThread()
    metaLock.acquire()
    try owner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
    finally metaLock.release()

  def isLocked(): Boolean =
    metaLock.acquire()
    try holdCount0 > 0
    finally metaLock.release()

  final def isFair(): Boolean =
    fair

  protected def getOwner(): Thread =
    metaLock.acquire()
    try owner.asInstanceOf[Thread]
    finally metaLock.release()

  override def toString(): String =
    val currentOwner = getOwner()
    val lockState =
      if currentOwner == null then "Unlocked"
      else "Locked by " + currentOwner.getName()
    s"${super.toString()}[$lockState]"

  private[locks] def pythonLock(): PyRLock =
    pyLock

  private[locks] def currentHoldCount(): Int =
    val current = Thread.currentThread()
    metaLock.acquire()
    try
      if owner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then holdCount0
      else 0
    finally metaLock.release()

  private[locks] def restoreAfterAwait(savedHoldCount: Int): Unit =
    val current = Thread.currentThread()
    metaLock.acquire()
    try
      owner = current
      holdCount0 = savedHoldCount
    finally metaLock.release()

private object ReentrantLock:
  final val PollMillis = 5L

  def timeoutToMillis(timeout: Long, unit: TimeUnit): Long =
    if timeout <= 0L then 0L
    else
      val millis = unit.toMillis(timeout)
      if millis > 0L then millis else 1L

private final class ReentrantCondition(lock: ReentrantLock) extends Condition:
  import ReentrantLock.*

  private val pyCondition = PyThreading.newCondition(lock.pythonLock())

  def await(): Unit =
    ensureHeld()
    if Thread.interrupted() then
      throw new InterruptedException(null)
    val savedHoldCount = lock.currentHoldCount()
    while true do
      val signalled = pyCondition.waitReady(PollMillis)
      lock.restoreAfterAwait(savedHoldCount)
      if Thread.interrupted() then
        throw new InterruptedException(null)
      if signalled then
        return

  def await(time: Long, unit: TimeUnit): Boolean =
    ensureHeld()
    if Thread.interrupted() then
      throw new InterruptedException(null)
    var remaining = timeoutToMillis(time, unit)
    if remaining <= 0L then
      return false
    val savedHoldCount = lock.currentHoldCount()
    while remaining > 0L do
      val slice =
        if remaining < PollMillis then remaining
        else PollMillis
      val signalled = pyCondition.waitReady(slice)
      lock.restoreAfterAwait(savedHoldCount)
      remaining -= slice
      if Thread.interrupted() then
        throw new InterruptedException(null)
      if signalled then
        return true
    false

  def awaitNanos(nanosTimeout: Long): Long =
    val timeoutMillis =
      if nanosTimeout <= 0L then 0L
      else
        val millis = TimeUnit.NANOSECONDS.toMillis(nanosTimeout)
        if millis > 0L then millis else 1L
    val started = System.nanoTime()
    val completed = await(timeoutMillis, TimeUnit.MILLISECONDS)
    val elapsed = System.nanoTime() - started
    if completed then
      nanosTimeout - elapsed
    else
      elapsed - nanosTimeout

  def awaitUntil(deadline: Date): Boolean =
    val remaining = deadline.getTime() - System.currentTimeMillis()
    await(remaining, TimeUnit.MILLISECONDS)

  def awaitUninterruptibly(): Unit =
    ensureHeld()
    val savedHoldCount = lock.currentHoldCount()
    var interrupted = false
    var signalled = false
    while !signalled do
      signalled = pyCondition.waitReady(PollMillis)
      lock.restoreAfterAwait(savedHoldCount)
      if !signalled && Thread.interrupted() then
        interrupted = true
    if Thread.interrupted() then
      interrupted = true
    if interrupted then
      Thread.currentThread().interrupt()

  def signal(): Unit =
    ensureHeld()
    pyCondition.notifyOne()

  def signalAll(): Unit =
    ensureHeld()
    pyCondition.notifyAllThreads()

  private def ensureHeld(): Unit =
    if !lock.isHeldByCurrentThread() then
      throw new IllegalMonitorStateException()
