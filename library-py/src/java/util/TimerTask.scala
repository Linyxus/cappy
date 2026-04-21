package java.util

import scala.language.unsafeNulls

import scala.python.runtime.PyTimer

abstract class TimerTask {
  private[util] var owner: Timer | Null = null
  private[util] var canceled: Boolean = false
  private[util] var scheduledOnceAndStarted: Boolean = false
  private[util] var lastScheduled: Long = 0L
  private[util] var handle: PyTimer | Null = null

  def run(): Unit

  def cancel(): Boolean =
    this.synchronized {
      val currentHandle = handle
      handle = null
      if currentHandle != null then
        currentHandle.cancel()

      if canceled || owner == null || scheduledOnceAndStarted then
        canceled = true
        false
      else
        canceled = true
        true
    }

  def scheduledExecutionTime(): Long =
    this.synchronized(lastScheduled)

  private[util] def isCanceled(): Boolean =
    this.synchronized(canceled)

  private[util] def markScheduledOnceStarted(): Unit =
    this.synchronized {
      scheduledOnceAndStarted = true
    }

  private[util] def timeout(delay: Long, body: Object): Unit =
    this.synchronized {
      if !canceled then
        val timer = scala.python.runtime.PyThreading.timer(delay, body)
        handle = timer
        timer.start()
    }

  private[util] def doRun(): Unit =
    val currentOwner =
      this.synchronized {
        owner.asInstanceOf[Timer]
      }
    if !isCanceled() && currentOwner != null && !currentOwner.isCanceled() then
      this.synchronized {
        lastScheduled = System.currentTimeMillis()
      }
      try
        run()
      catch
        case t: Throwable =>
          this.synchronized {
            canceled = true
          }
          throw t
}
