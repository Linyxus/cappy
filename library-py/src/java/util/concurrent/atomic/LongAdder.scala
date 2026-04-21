package java.util.concurrent.atomic

import java.io.Serializable
import scala.python.runtime.PyThreading

class LongAdder extends Number with Serializable:
  private var value: Long = 0L
  private val lock = PyThreading.newLock()

  final def add(x: Long): Unit =
    lock.lock()
    try
      value = value + x
    finally
      lock.unlock()

  final def increment(): Unit =
    add(1L)

  final def decrement(): Unit =
    add(-1L)

  final def sum(): Long =
    value

  final def reset(): Unit =
    lock.lock()
    try
      value = 0L
    finally
      lock.unlock()

  final def sumThenReset(): Long =
    lock.lock()
    try
      val result = value
      value = 0L
      result
    finally
      lock.unlock()

  override def toString(): String =
    String.valueOf(value)

  final def longValue(): Long =
    value

  final def intValue(): Int =
    value.toInt

  final def floatValue(): Float =
    value.toFloat

  final def doubleValue(): Double =
    value.toDouble
