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

  def cancel(): Boolean = {
    if handle != null then
      handle.cancel()
      handle = null

    if canceled || owner == null || scheduledOnceAndStarted then
      canceled = true
      false
    else
      canceled = true
      true
  }

  def scheduledExecutionTime(): Long =
    lastScheduled

  private[util] def timeout(delay: Long, body: Object): Unit = {
    if !canceled then
      val timer = scala.python.runtime.PyThreading.timer(delay, body)
      handle = timer
      timer.start()
  }

  private[util] def doRun(): Unit = {
    val currentOwner = owner.asInstanceOf[Timer]
    if !canceled && !currentOwner.canceled then
      lastScheduled = System.currentTimeMillis()
      try
        run()
      catch
        case t: Throwable =>
          canceled = true
          throw t
  }
}
