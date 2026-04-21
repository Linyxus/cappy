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

package java.lang

import java.util.function.Supplier
import scala.python.runtime.PyThreading

class ThreadLocal[T]:
  private val localState = PyThreading.newLocal()

  protected def initialValue(): T = null.asInstanceOf[T]

  def get(): T =
    if !hasCurrentValue() then
      val initial = initialValue()
      installValue(initial)
      initial
    else
      currentValue()

  def set(o: T): Unit =
    installValue(o)

  def remove(): Unit =
    if localState.hasAttr(ThreadLocal.PresentAttr) then
      localState.delAttr(ThreadLocal.PresentAttr)
    if localState.hasAttr(ThreadLocal.ValueAttr) then
      localState.delAttr(ThreadLocal.ValueAttr)

  private[java] final def hasCurrentValue(): scala.Boolean =
    localState.hasAttr(ThreadLocal.PresentAttr)

  private[java] final def snapshotCurrentValue(): Any =
    currentValue().asInstanceOf[Any]

  private[java] final def installValue(value: Any): Unit =
    localState.setAttr(ThreadLocal.ValueAttr, value)
    localState.setAttr(ThreadLocal.PresentAttr, true)

  private def currentValue(): T =
    localState.getAttr(ThreadLocal.ValueAttr).asInstanceOf[T]

object ThreadLocal:
  private final val PresentAttr = "present"
  private final val ValueAttr = "value"

  def withInitial[T](supplier: Supplier[? <: T]): ThreadLocal[T] =
    val nnSupplier = ThrowablesSupport.requireNonNull(supplier)
    new ThreadLocal[T]:
      override protected def initialValue(): T =
        nnSupplier.get()
