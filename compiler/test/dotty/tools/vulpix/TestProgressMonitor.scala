package dotty
package tools
package vulpix

import java.io.PrintStream
import java.lang.management.ManagementFactory
import java.util.{Timer, TimerTask}

import scala.jdk.CollectionConverters.*

/** Timer-driven progress bar shared by long-running test harnesses. */
final class TestProgressMonitor(
    total: Int,
    completed: () => Int,
    failed: () => Int,
    out: PrintStream = System.out,
    isInteractive: Boolean = TestProgressMonitor.isInteractive,
    suppressAllOutput: Boolean = false
):
  private var timer: Timer | Null = null
  private var startMillis: Long = 0L

  def start(): Unit =
    if enabled then
      startMillis = System.currentTimeMillis()
      val active = new Timer()
      timer = active
      active.schedule((() => update()): TimerTask, 100 /*ms*/, 200 /*ms*/)

  def finish(): Unit =
    timer match
      case active: Timer =>
        active.cancel()
        out.println(s"\r${makeProgressBar}")
        timer = null
      case null => ()

  private def enabled: Boolean =
    total > 0 && isInteractive && !suppressAllOutput

  private def update(): Unit =
    if completed() < total && !TestProgressMonitor.isUserDebugging then
      out.print(s"\r${makeProgressBar}")

  private def makeProgressBar: String =
    val done = completed()
    val timestamp = (System.currentTimeMillis - startMillis) / 1000
    val progress = (done.toDouble / total * 40).toInt
    val past = "=" * math.max(progress - 1, 0)
    val curr = if progress > 0 then ">" else ""
    val next = " " * (40 - progress)
    s"[$past$curr$next] completed ($done/$total, ${failed()} failed, ${timestamp}s)"

object TestProgressMonitor:
  val isInteractive: Boolean =
    val interactiveProp = System.getProperty("dotty.tests.interactive")
    (interactiveProp == null || interactiveProp == "TRUE") && !sys.env.contains("DOTTY_CI_RUN")

  def isUserDebugging: Boolean =
    val mxBean = ManagementFactory.getRuntimeMXBean
    mxBean.getInputArguments.asScala.exists(_.contains("jdwp"))
