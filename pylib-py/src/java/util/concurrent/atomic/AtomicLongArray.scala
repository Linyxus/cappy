package java.util.concurrent.atomic

import scala.python.runtime.PyThreading

class AtomicLongArray(initialLength: Int) extends Serializable:
  private val inner = new Array[Long](initialLength)
  private val lock = PyThreading.newLock()

  def this(array: Array[Long]) =
    this(array.length)
    System.arraycopy(array, 0, inner, 0, array.length)

  final def length(): Int =
    inner.length

  final def get(i: Int): Long =
    inner(i)

  final def set(i: Int, newValue: Long): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: Long): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: Long): Long =
    lock.lock()
    try
      val old = inner(i)
      inner(i) = newValue
      old
    finally
      lock.unlock()

  final def compareAndSet(i: Int, expect: Long, update: Long): Boolean =
    lock.lock()
    try
      if inner(i) != expect then
        false
      else
        inner(i) = update
        true
    finally
      lock.unlock()

  final def weakCompareAndSet(i: Int, expect: Long, update: Long): Boolean =
    compareAndSet(i, expect, update)

  final def getAndIncrement(i: Int): Long =
    getAndAdd(i, 1L)

  final def getAndDecrement(i: Int): Long =
    getAndAdd(i, -1L)

  final def getAndAdd(i: Int, delta: Long): Long =
    lock.lock()
    try
      val old = inner(i)
      inner(i) = old + delta
      old
    finally
      lock.unlock()

  final def incrementAndGet(i: Int): Long =
    addAndGet(i, 1L)

  final def decrementAndGet(i: Int): Long =
    addAndGet(i, -1L)

  final def addAndGet(i: Int, delta: Long): Long =
    lock.lock()
    try
      inner(i) = inner(i) + delta
      inner(i)
    finally
      lock.unlock()

  override def toString(): String =
    java.util.Arrays.toString(inner)
