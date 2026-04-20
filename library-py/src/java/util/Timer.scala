package java.util

import scala.language.unsafeNulls

class Timer() {
  private[util] var canceled: Boolean = false

  def this(isDaemon: Boolean) = this()

  def this(name: String) = this()

  def this(name: String, isDaemon: Boolean) = this()

  private def acquire(task: TimerTask): Unit = {
    if canceled then
      throw new IllegalStateException("Timer already cancelled.")
    else if task.owner != null || task.canceled then
      throw new IllegalStateException("TimerTask already scheduled or canceled.")
    task.owner = this
  }

  private def checkDelay(delay: Long): Unit = {
    if delay < 0 || delay + System.currentTimeMillis() < 0 then
      throw new IllegalArgumentException("Negative delay.")
  }

  private def checkTime(time: Date): Unit = {
    if time.getTime() < 0 then
      throw new IllegalArgumentException(s"Negative time: $time.")
  }

  private def checkPeriod(period: Long): Unit = {
    if period <= 0 then
      throw new IllegalArgumentException("Non-positive period.")
  }

  private def getMillisUntil(time: Date): Long =
    Math.max(0L, time.getTime() - System.currentTimeMillis())

  private def scheduleOnce(task: TimerTask, delay: Long): Unit = {
    acquire(task)
    task.timeout(delay, new RunOnce(task))
  }

  private def schedulePeriodicStep(task: TimerTask, period: Long): Unit = {
    if !task.canceled && !canceled then
      val started = System.currentTimeMillis()
      task.doRun()
      if !task.canceled && !canceled then
        val elapsed = System.currentTimeMillis() - started
        val nextDelay = Math.max(0L, period - elapsed)
        task.timeout(nextDelay, new FixedDelayStep(task, period))
  }

  private def scheduleFixedRateStep(task: TimerTask, period: Long, scheduledTime: Long): Unit = {
    if !task.canceled && !canceled then
      task.doRun()
      if !task.canceled && !canceled then
        val nextScheduled = scheduledTime + period
        val nextDelay = Math.max(0L, nextScheduled - System.currentTimeMillis())
        task.timeout(nextDelay, new FixedRateStep(task, period, nextScheduled))
  }

  def schedule(task: TimerTask, delay: Long): Unit = {
    checkDelay(delay)
    scheduleOnce(task, delay)
  }

  def schedule(task: TimerTask, time: Date): Unit = {
    checkTime(time)
    scheduleOnce(task, getMillisUntil(time))
  }

  def schedule(task: TimerTask, delay: Long, period: Long): Unit = {
    checkDelay(delay)
    checkPeriod(period)
    acquire(task)
    task.timeout(delay, new FixedDelayStep(task, period))
  }

  def schedule(task: TimerTask, firstTime: Date, period: Long): Unit = {
    checkTime(firstTime)
    checkPeriod(period)
    acquire(task)
    task.timeout(getMillisUntil(firstTime), new FixedDelayStep(task, period))
  }

  def scheduleAtFixedRate(task: TimerTask, delay: Long, period: Long): Unit = {
    checkDelay(delay)
    checkPeriod(period)
    acquire(task)
    task.timeout(delay, new FixedRateStep(task, period, System.currentTimeMillis() + period))
  }

  def scheduleAtFixedRate(task: TimerTask, firstTime: Date, period: Long): Unit = {
    checkTime(firstTime)
    checkPeriod(period)
    acquire(task)
    val delay = getMillisUntil(firstTime)
    task.timeout(delay, new FixedRateStep(task, period, System.currentTimeMillis() + period))
  }

  def cancel(): Unit =
    canceled = true

  def purge(): Int = 0

  private final class RunOnce(task: TimerTask):
    def `__call__`(): Unit =
      task.scheduledOnceAndStarted = true
      task.doRun()

  private final class FixedDelayStep(task: TimerTask, period: Long):
    def `__call__`(): Unit =
      schedulePeriodicStep(task, period)

  private final class FixedRateStep(task: TimerTask, period: Long, scheduledTime: Long):
    def `__call__`(): Unit =
      scheduleFixedRateStep(task, period, scheduledTime)
}
