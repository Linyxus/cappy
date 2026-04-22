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

import scala.python.runtime.PySys

class Runtime private ():
  def availableProcessors(): Int = 1

  def gc(): Unit =
    ()

  def exit(status: Int): Unit =
    PySys.exit(status)

  // Stubs — Python doesn't have JVM-style shutdown hooks. Tests
  // referencing these never expect them to fire.
  def addShutdownHook(hook: Thread): Unit = ()
  def removeShutdownHook(hook: Thread): scala.Boolean = false

object Runtime:
  private val currentRuntime = new Runtime

  def getRuntime(): Runtime =
    currentRuntime
