package java.util.concurrent.atomic

import java.util.function.{LongBinaryOperator, LongUnaryOperator}
import scala.python.runtime.PyThreading

class AtomicLong(private var value: Long) extends Number with Serializable:
  private val lock = PyThreading.newLock()

  def this() =
    this(0L)

  final def get(): Long =
    value

  final def set(newValue: Long): Unit =
    value = newValue

  final def lazySet(newValue: Long): Unit =
    set(newValue)

  final def getAndSet(newValue: Long): Long =
    lock.lock()
    try
      val old = value
      value = newValue
      old
    finally
      lock.unlock()

  final def compareAndSet(expect: Long, update: Long): Boolean =
    lock.lock()
    try
      if expect != value then
        false
      else
        value = update
        true
    finally
      lock.unlock()

  final def weakCompareAndSet(expect: Long, update: Long): Boolean =
    compareAndSet(expect, update)

  final def getAndIncrement(): Long =
    getAndAdd(1L)

  final def getAndDecrement(): Long =
    getAndAdd(-1L)

  final def getAndAdd(delta: Long): Long =
    lock.lock()
    try
      val old = value
      value = old + delta
      old
    finally
      lock.unlock()

  final def incrementAndGet(): Long =
    addAndGet(1L)

  final def decrementAndGet(): Long =
    addAndGet(-1L)

  final def addAndGet(delta: Long): Long =
    lock.lock()
    try
      val newValue = value + delta
      value = newValue
      newValue
    finally
      lock.unlock()

  final def getAndUpdate(updateFunction: LongUnaryOperator): Long =
    lock.lock()
    try
      val old = value
      value = updateFunction.applyAsLong(old)
      old
    finally
      lock.unlock()

  final def updateAndGet(updateFunction: LongUnaryOperator): Long =
    lock.lock()
    try
      value = updateFunction.applyAsLong(value)
      value
    finally
      lock.unlock()

  final def getAndAccumulate(x: Long, accumulatorFunction: LongBinaryOperator): Long =
    lock.lock()
    try
      val old = value
      value = accumulatorFunction.applyAsLong(old, x)
      old
    finally
      lock.unlock()

  final def accumulateAndGet(x: Long, accumulatorFunction: LongBinaryOperator): Long =
    lock.lock()
    try
      value = accumulatorFunction.applyAsLong(value, x)
      value
    finally
      lock.unlock()

  override def toString(): String =
    value.toString()

  def intValue(): Int = value.toInt
  def longValue(): Long = value
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
