package java.util.concurrent.atomic

import java.util.function.{BinaryOperator, UnaryOperator}
import scala.python.runtime.PyThreading

class AtomicReference[T <: AnyRef](private var value: T) extends Serializable:
  private val lock = PyThreading.newLock()

  def this() =
    this(null.asInstanceOf[T])

  final def get(): T =
    value

  final def set(newValue: T): Unit =
    value = newValue

  final def lazySet(newValue: T): Unit =
    set(newValue)

  final def compareAndSet(expect: T, update: T): Boolean =
    lock.lock()
    try
      if expect ne value then
        false
      else
        value = update
        true
    finally
      lock.unlock()

  final def weakCompareAndSet(expect: T, update: T): Boolean =
    compareAndSet(expect, update)

  final def getAndSet(newValue: T): T =
    lock.lock()
    try
      val old = value
      value = newValue
      old
    finally
      lock.unlock()

  final def getAndUpdate(updateFunction: UnaryOperator[T]): T =
    lock.lock()
    try
      val old = value
      value = updateFunction.apply(old)
      old
    finally
      lock.unlock()

  final def updateAndGet(updateFunction: UnaryOperator[T]): T =
    lock.lock()
    try
      value = updateFunction.apply(value)
      value
    finally
      lock.unlock()

  final def getAndAccumulate(x: T, accumulatorFunction: BinaryOperator[T]): T =
    lock.lock()
    try
      val old = value
      value = accumulatorFunction.apply(old, x)
      old
    finally
      lock.unlock()

  final def accumulateAndGet(x: T, accumulatorFunction: BinaryOperator[T]): T =
    lock.lock()
    try
      value = accumulatorFunction.apply(value, x)
      value
    finally
      lock.unlock()

  override def toString(): String =
    String.valueOf(value)
