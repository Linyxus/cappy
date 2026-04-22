package java.util.concurrent.atomic

import java.util.function.{IntBinaryOperator, IntUnaryOperator}
import scala.python.runtime.PyThreading

class AtomicInteger(private var value: Int) extends Number with Serializable:
  private val lock = PyThreading.newLock()

  def this() =
    this(0)

  final def get(): Int =
    value

  final def set(newValue: Int): Unit =
    value = newValue

  final def lazySet(newValue: Int): Unit =
    set(newValue)

  final def getAndSet(newValue: Int): Int =
    lock.lock()
    try
      val old = value
      value = newValue
      old
    finally
      lock.unlock()

  final def compareAndSet(expect: Int, update: Int): Boolean =
    lock.lock()
    try
      if expect != value then
        false
      else
        value = update
        true
    finally
      lock.unlock()

  final def weakCompareAndSet(expect: Int, update: Int): Boolean =
    compareAndSet(expect, update)

  final def getAndIncrement(): Int =
    getAndAdd(1)

  final def getAndDecrement(): Int =
    getAndAdd(-1)

  final def getAndAdd(delta: Int): Int =
    lock.lock()
    try
      val old = value
      value = old + delta
      old
    finally
      lock.unlock()

  final def incrementAndGet(): Int =
    addAndGet(1)

  final def decrementAndGet(): Int =
    addAndGet(-1)

  final def addAndGet(delta: Int): Int =
    lock.lock()
    try
      val newValue = value + delta
      value = newValue
      newValue
    finally
      lock.unlock()

  final def getAndUpdate(updateFunction: IntUnaryOperator): Int =
    lock.lock()
    try
      val old = value
      value = updateFunction.applyAsInt(old)
      old
    finally
      lock.unlock()

  final def updateAndGet(updateFunction: IntUnaryOperator): Int =
    lock.lock()
    try
      value = updateFunction.applyAsInt(value)
      value
    finally
      lock.unlock()

  final def getAndAccumulate(x: Int, accumulatorFunction: IntBinaryOperator): Int =
    lock.lock()
    try
      val old = value
      value = accumulatorFunction.applyAsInt(old, x)
      old
    finally
      lock.unlock()

  final def accumulateAndGet(x: Int, accumulatorFunction: IntBinaryOperator): Int =
    lock.lock()
    try
      value = accumulatorFunction.applyAsInt(value, x)
      value
    finally
      lock.unlock()

  override def toString(): String =
    value.toString()

  def intValue(): Int = value
  def longValue(): Long = value.toLong
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
