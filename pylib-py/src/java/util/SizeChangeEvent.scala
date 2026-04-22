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

private[util] trait SizeChangeEvent {
  protected def end: Int
  protected def end_=(newEnd: Int): Unit

  @inline
  protected final def changeSize(delta: Int): Unit = {
    end = end + delta
    onSizeChanged(delta)
  }

  protected def onSizeChanged(delta: Int): Unit = () // override if needed
}
