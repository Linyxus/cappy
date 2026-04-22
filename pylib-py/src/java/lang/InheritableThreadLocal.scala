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

class InheritableThreadLocal[T] extends ThreadLocal[T]:
  Thread.registerInheritable(this)

  protected def childValue(parentValue: T): T = parentValue

  private[java] final def snapshotForChild(): Thread.InheritedValue | Null =
    if !hasCurrentValue() then
      null
    else
      new Thread.InheritedValue(this, childValue(snapshotCurrentValue().asInstanceOf[T]))

  private[java] final def installInheritedValue(value: Any): Unit =
    installValue(value.asInstanceOf[T])
