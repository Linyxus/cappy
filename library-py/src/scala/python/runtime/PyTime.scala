package scala.python.runtime

import scala.python.{PyAny, extern, native}

object PyTime:
  @extern("time")
  private object time extends PyAny:
    def time_ns(): Long = native
    def perf_counter_ns(): Long = native
    def sleep(seconds: Double): Unit = native

  def time_ns(): Long =
    time.time_ns()

  def perf_counter_ns(): Long =
    time.perf_counter_ns()

  def sleep(seconds: Double): Unit =
    time.sleep(seconds)
