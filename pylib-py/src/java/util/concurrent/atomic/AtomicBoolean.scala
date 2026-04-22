package java.util.concurrent.atomic

import scala.python.runtime.PyThreading

class AtomicBoolean(private var value: Boolean) extends Serializable:
  private val lock = PyThreading.newLock()

  def this() =
    this(false)

  final def get(): Boolean =
    value

  final def compareAndSet(expect: Boolean, update: Boolean): Boolean =
    lock.lock()
    try
      if expect != value then
        false
      else
        value = update
        true
    finally
      lock.unlock()

  def weakCompareAndSet(expect: Boolean, update: Boolean): Boolean =
    compareAndSet(expect, update)

  final def set(newValue: Boolean): Unit =
    value = newValue

  final def lazySet(newValue: Boolean): Unit =
    set(newValue)

  final def getAndSet(newValue: Boolean): Boolean =
    lock.lock()
    try
      val old = value
      value = newValue
      old
    finally
      lock.unlock()

  override def toString(): String =
    value.toString()
