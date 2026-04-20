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

class Thread private (dummy: Unit) extends Runnable:
  private var interruptedState = false
  private var name: String = "main"

  def run(): Unit = ()

  def interrupt(): Unit =
    interruptedState = true

  def isInterrupted(): scala.Boolean =
    interruptedState

  final def setName(name: String): Unit =
    this.name = name

  final def getName(): String =
    this.name

  def getStackTrace(): Array[StackTraceElement] =
    StackTrace.getCurrentStackTrace()

  def getId(): scala.Long = 1

object Thread:
  private val SingleThread = new Thread(())

  def currentThread(): Thread = SingleThread

  def interrupted(): scala.Boolean =
    val ret = currentThread().isInterrupted()
    currentThread().interruptedState = false
    ret
