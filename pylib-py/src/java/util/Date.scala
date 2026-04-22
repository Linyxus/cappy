package java.util

import java.lang.Cloneable

import scala.language.unsafeNulls

class Date(private var millis: Long)
    extends Object with Serializable with Cloneable with Comparable[Date] {

  def this() = this(System.currentTimeMillis())

  @Deprecated
  def this(date: String) = this(Date.parse(date))

  def after(when: Date): Boolean =
    millis > when.millis

  def before(when: Date): Boolean =
    millis < when.millis

  override def clone(): Object =
    new Date(millis)

  override def compareTo(anotherDate: Date): Int =
    java.lang.Long.compare(millis, anotherDate.millis)

  override def equals(obj: Any): Boolean =
    obj != null && millis == obj.asInstanceOf[Date].getTime()

  override def hashCode(): Int =
    millis.hashCode()

  def getTime(): Long =
    millis

  def setTime(time: Long): Unit =
    millis = time

  override def toString(): String =
    s"Date($millis)"
}

object Date:
  @Deprecated
  def parse(string: String): Long =
    java.lang.Long.parseLong(string)
