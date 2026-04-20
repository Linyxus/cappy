/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util

abstract class AbstractSet[E] protected () extends AbstractCollection[E] with Set[E] {
  override def equals(that: Any): Boolean = {
    if (that.asInstanceOf[AnyRef] eq this) true
    else {
      that match {
        case that: Collection[_] => that.size() == this.size() && containsAll(that)
        case _                   => false
      }
    }
  }

  override def hashCode(): Int = {
    val iter = iterator()
    var hash = 0
    while (iter.hasNext()) {
      hash += Objects.hashCode(iter.next())
    }
    hash
  }

  override def removeAll(c: Collection[_]): Boolean = {
    if (size() > c.size()) {
      val iter = c.iterator()
      var changed = false
      while (iter.hasNext()) {
        if (this.remove(iter.next()))
          changed = true
      }
      changed
    } else {
      val iter = this.iterator()
      var changed = false
      while (iter.hasNext()) {
        if (c.contains(iter.next())) {
          iter.remove()
          changed = true
        }
      }
      changed
    }
  }
}
