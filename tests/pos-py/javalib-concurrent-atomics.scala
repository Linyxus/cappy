import java.util.concurrent.atomic.*
import java.util.function.*
import scala.python.runtime.PyThreading

final class PlusTen extends IntUnaryOperator:
  def applyAsInt(value: Int): Int = value + 10

final class TimesInt extends IntBinaryOperator:
  def applyAsInt(left: Int, right: Int): Int = left * right

final class PlusHundred extends LongUnaryOperator:
  def applyAsLong(value: Long): Long = value + 100L

final class AddLong extends LongBinaryOperator:
  def applyAsLong(left: Long, right: Long): Long = left + right

final class Bang extends UnaryOperator[String]:
  def apply(value: String): String = value + "!"

final class MergeStrings extends BinaryOperator[String]:
  def apply(left: String, right: String): String = left + right

@main def javalibConcurrentAtomics(): Unit =
  val atomicBoolean = new AtomicBoolean()
  println("atomicboolean:" + atomicBoolean.compareAndSet(false, true) + ":" + atomicBoolean.getAndSet(false) + ":" + atomicBoolean.get())

  val atomicInt = new AtomicInteger(5)
  val intUpdated = atomicInt.updateAndGet(new PlusTen())
  val intAccumulated = atomicInt.accumulateAndGet(2, new TimesInt())
  println("atomicint:" + intUpdated + ":" + intAccumulated + ":" + atomicInt.getAndSet(3) + ":" + atomicInt.get())

  val atomicLong = new AtomicLong(7L)
  val longUpdated = atomicLong.updateAndGet(new PlusHundred())
  val longAccumulated = atomicLong.accumulateAndGet(5L, new AddLong())
  println("atomiclong:" + longUpdated + ":" + longAccumulated + ":" + atomicLong.getAndSet(9L) + ":" + atomicLong.get())

  val atomicRef = new AtomicReference[String]("a")
  val refUpdated = atomicRef.updateAndGet(new Bang())
  val refAccumulated = atomicRef.accumulateAndGet("b", new MergeStrings())
  val currentRef = atomicRef.get()
  println("atomicref:" + refUpdated + ":" + refAccumulated + ":" + atomicRef.compareAndSet(currentRef, "done") + ":" + atomicRef.get())

  val atomicLongArray = new AtomicLongArray(Array[Long](1L, 2L))
  println("atomiclongarray:" + atomicLongArray.getAndAdd(0, 4L) + ":" + atomicLongArray.compareAndSet(1, 2L, 9L) + ":" + atomicLongArray.get(0) + ":" + atomicLongArray.get(1))

  val atomicRefArray = new AtomicReferenceArray[String](Array("x", "y"))
  println("atomicrefarray:" + atomicRefArray.getAndSet(0, "z") + ":" + atomicRefArray.compareAndSet(1, "y", "done") + ":" + atomicRefArray.get(0) + ":" + atomicRefArray.get(1))

  val start = PyThreading.newEvent()
  val stressed = new AtomicInteger(0)
  val adder = new LongAdder()
  val threads = new Array[java.lang.Thread](16)

  def worker(): Unit =
    start.waitReady()
    var i = 0
    while i < 5000 do
      stressed.incrementAndGet()
      adder.increment()
      i += 1

  var index = 0
  while index < threads.length do
    threads(index) = new java.lang.Thread(() => worker(), "atomic-" + (index + 1))
    threads(index).start()
    index += 1

  start.set()

  index = 0
  while index < threads.length do
    threads(index).join()
    index += 1

  println("atomicint-stress:" + stressed.get())
  println("longadder:" + adder.sum() + ":" + adder.sumThenReset() + ":" + adder.sum())
