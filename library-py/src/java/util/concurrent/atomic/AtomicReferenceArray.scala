package java.util.concurrent.atomic

import scala.python.runtime.PyThreading

class AtomicReferenceArray[E <: AnyRef](initialLength: Int) extends Serializable:
  private val inner = new Array[AnyRef](initialLength)
  private val lock = PyThreading.newLock()

  def this(array: Array[E]) =
    this(array.length)
    System.arraycopy(array, 0, inner, 0, array.length)

  final def length(): Int =
    inner.length

  final def get(i: Int): E =
    inner(i).asInstanceOf[E]

  final def set(i: Int, newValue: E): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: E): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: E): E =
    lock.lock()
    try
      val old = inner(i).asInstanceOf[E]
      inner(i) = newValue
      old
    finally
      lock.unlock()

  final def compareAndSet(i: Int, expect: E, update: E): Boolean =
    lock.lock()
    try
      if inner(i).asInstanceOf[E] ne expect then
        false
      else
        inner(i) = update
        true
    finally
      lock.unlock()

  final def weakCompareAndSet(i: Int, expect: E, update: E): Boolean =
    compareAndSet(i, expect, update)

  override def toString(): String =
    java.util.Arrays.toString(inner)
