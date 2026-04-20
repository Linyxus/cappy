package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyThreading:
  @extern("threading")
  private object threading extends PyAny:
    @name("Timer")
    def newTimer(intervalSeconds: Double, function: Any): PyDynamic = native

  def timer(intervalMillis: Long, function: Any): PyTimer =
    new PyTimer(threading.newTimer(intervalMillis.toDouble / 1000.0, function))

final class PyTimer private[runtime] (private val underlying: PyDynamic):
  def start(): Unit =
    underlying.start()

  def cancel(): Unit =
    underlying.cancel()
