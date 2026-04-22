package java.util.concurrent.locks

import java.util.Date
import java.util.concurrent.TimeUnit

trait Condition:
  def await(): Unit
  def await(time: Long, unit: TimeUnit): Boolean
  def awaitNanos(nanosTimeout: Long): Long
  def awaitUntil(deadline: Date): Boolean
  def awaitUninterruptibly(): Unit
  def signal(): Unit
  def signalAll(): Unit
