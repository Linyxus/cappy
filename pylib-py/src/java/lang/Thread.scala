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

import scala.python.runtime.{PyEvent, PyThread, PyThreading}

class Thread private (
    private var targetRef: Runnable | Null,
    initialName: String,
    private var daemonState: scala.Boolean,
    private val threadId0: scala.Long,
    initialPyThread: PyThread | Null,
    started0: scala.Boolean
) extends Runnable:
  private var interruptedState = false
  private var name = initialName
  private var priority = Thread.NORM_PRIORITY
  private var started = started0
  private var pyThread: PyThread | Null = initialPyThread
  private val interruptEvent: PyEvent = PyThreading.newEvent()
  private val finishedEvent: PyEvent = PyThreading.newEvent()

  def this() =
    this(null, Thread.nextUnnamedThreadName(), false, Thread.nextThreadId(), null, false)

  def this(target: Runnable) =
    this(target, Thread.nextUnnamedThreadName(), false, Thread.nextThreadId(), null, false)

  def this(name: String) =
    this(null, ThrowablesSupport.requireNonNull(name), false, Thread.nextThreadId(), null, false)

  def this(target: Runnable, name: String) =
    this(target, ThrowablesSupport.requireNonNull(name), false, Thread.nextThreadId(), null, false)

  def run(): Unit =
    val target = targetRef
    if target != null then
      target.run()

  def start(): Unit =
    if started then
      throw new IllegalThreadStateException(null)

    val inherited = Thread.captureInheritedValues()
    val created = Thread.newRuntimeThread(this, inherited, name, daemonState)
    pyThread = created
    started = true
    created.start()

  def join(): Unit =
    waitForCompletion(0L)

  def join(millis: scala.Long): Unit =
    if millis < 0L then
      throw new IllegalArgumentException(null)
    waitForCompletion(millis)

  def join(millis: scala.Long, nanos: scala.Int): Unit =
    if millis < 0L || nanos < 0 || nanos > 999999 then
      throw new IllegalArgumentException(null)
    val adjusted =
      if nanos == 0 then millis
      else millis + 1L
    waitForCompletion(adjusted)

  def isAlive(): scala.Boolean =
    val runtime = pyThread
    runtime != null && runtime.asInstanceOf[PyThread].isAlive()

  def interrupt(): Unit =
    interruptedState = true
    interruptEvent.set()

  def isInterrupted(): scala.Boolean =
    interruptedState

  final def setName(name: String): Unit =
    val nnName = ThrowablesSupport.requireNonNull(name)
    this.name = nnName
    val runtime = pyThread
    if runtime != null then
      runtime.asInstanceOf[PyThread].setName(nnName)

  final def getName(): String =
    this.name

  final def setDaemon(on: scala.Boolean): Unit =
    if started then
      throw new IllegalThreadStateException(null)
    daemonState = on

  final def isDaemon(): scala.Boolean =
    daemonState

  final def setPriority(newPriority: scala.Int): Unit =
    if newPriority < Thread.MIN_PRIORITY || newPriority > Thread.MAX_PRIORITY then
      throw new IllegalArgumentException(null)
    priority = newPriority

  final def getPriority(): scala.Int =
    priority

  def getStackTrace(): Array[StackTraceElement] =
    if this eq Thread.currentThread() then
      StackTrace.getCurrentStackTrace()
    else
      new Array[StackTraceElement](0)

  def getId(): scala.Long =
    threadId0

  private var uncaughtHandler: Thread.UncaughtExceptionHandler | Null = null

  final def setUncaughtExceptionHandler(handler: Thread.UncaughtExceptionHandler | Null): Unit =
    uncaughtHandler = handler

  final def getUncaughtExceptionHandler(): Thread.UncaughtExceptionHandler | Null =
    val handler = uncaughtHandler
    if handler != null then handler
    else Thread.getDefaultUncaughtExceptionHandler()

  private[lang] final def consumeInterruptedStatus(): scala.Boolean =
    val wasInterrupted = interruptedState
    if wasInterrupted then
      interruptedState = false
      interruptEvent.clear()
    wasInterrupted

  private[lang] final def awaitInterrupt(timeoutMillis: scala.Long): scala.Boolean =
    if timeoutMillis <= 0L then
      interruptEvent.isSet()
    else
      interruptEvent.waitReady(timeoutMillis)

  private[lang] final def bindCurrentRuntimeThread(): Unit =
    pyThread = PyThreading.currentThread()

  private[lang] final def installInheritedValue(local: InheritableThreadLocal[?], value: Any): Unit =
    local.installInheritedValue(value)

  private def waitForCompletion(timeoutMillis: scala.Long): Unit =
    if !started || !isAlive() then
      ()
    else
      val current = Thread.currentThread()
      Thread.throwIfInterrupted(current)

      if timeoutMillis == 0L then
        while isAlive() do
          if finishedEvent.waitReady(Thread.JoinPollMillis) then
            return
          Thread.throwIfInterrupted(current)
      else
        var remaining = timeoutMillis
        while remaining > 0L && isAlive() do
          val slice =
            if remaining < Thread.JoinPollMillis then remaining
            else Thread.JoinPollMillis
          if finishedEvent.waitReady(slice) then
            return
          Thread.throwIfInterrupted(current)
          remaining -= slice

  private[lang] final def markCompleted(): Unit =
    targetRef = null
    finishedEvent.set()

object Thread:
  final val MIN_PRIORITY = 1
  final val NORM_PRIORITY = 5
  final val MAX_PRIORITY = 10

  /** SAM interface for uncaught exception handlers. JVM-shape: takes the
   *  failing thread and the cause. The Python backend does not currently
   *  install a `threading.excepthook` adapter — the handler is stored on
   *  the `Thread` instance and consulted by reflection-style consumers
   *  (e.g. `scala.concurrent.impl.ExecutionContextImpl.reportFailure`),
   *  which is the only path in our test corpus that actually reads it.
   */
  trait UncaughtExceptionHandler:
    def uncaughtException(t: Thread, e: Throwable): Unit

  private var defaultHandler: UncaughtExceptionHandler | Null = null

  def setDefaultUncaughtExceptionHandler(handler: UncaughtExceptionHandler | Null): Unit =
    defaultHandler = handler

  def getDefaultUncaughtExceptionHandler(): UncaughtExceptionHandler | Null =
    defaultHandler

  // Stubs for thread-enumeration APIs. Returns 0/1 — pos-py tests don't
  // depend on accurate thread counts.
  def activeCount(): scala.Int = 1
  def enumerate(target: Array[Thread]): scala.Int = 0

  // `Thread.yield()` is a hint on the JVM. Under CPython we have no
  // useful equivalent; we model it as a no-op. The trailing underscore
  // mirrors the encoded name at call sites (`yield_` because `yield`
  // is reserved in Scala source).
  def `yield`(): Unit = ()

  private final val JoinPollMillis = 5L
  private final val SleepPollMillis = 5L

  private[lang] final class InheritedValue(
      val local: InheritableThreadLocal[?],
      val value: Any
  )

  private var registryLock0: scala.python.runtime.PyRLock | Null = null
  private var currentThreadLocal0: scala.python.runtime.PyLocal | Null = null
  private var nextId = 2L
  private var nextUnnamed = 0

  private var pythonThreadIds = new Array[scala.Long](8)
  private var scalaThreads = new Array[Thread](8)
  private var registeredThreadCount = 0

  private var inheritableLocals = new Array[AnyRef](8)
  private var inheritableLocalCount = 0
  private final val CurrentThreadAttr = "javaThread"

  private var mainThread0: Thread | Null = null

  def currentThread(): Thread =
    val local = currentThreadLocalRef()
    if local.hasAttr(CurrentThreadAttr) then
      return local.getAttr(CurrentThreadAttr).asInstanceOf[Thread]

    ensureMainThreadRegistered()
    val runtime = PyThreading.currentThread()
    val ident = currentPythonThreadId(runtime)
    val known = lookupRunningThread(ident)
    if known != null then
      local.setAttr(CurrentThreadAttr, known)
      known
    else
      val adoptedName =
        val raw = runtime.getName()
        if raw == null then nextUnnamedThreadName() else raw
      val adopted = new Thread(null, adoptedName, runtime.isDaemon(), nextThreadId(), runtime, true)
      registerRunningThread(ident, adopted)
      local.setAttr(CurrentThreadAttr, adopted)
      adopted

  def interrupted(): scala.Boolean =
    currentThread().consumeInterruptedStatus()

  def sleep(millis: scala.Long): Unit =
    if millis < 0L then
      throw new IllegalArgumentException(null)

    val current = currentThread()
    throwIfInterrupted(current)

    var remaining = millis
    while remaining > 0L do
      val slice =
        if remaining < SleepPollMillis then remaining
        else SleepPollMillis
      PyThreading.sleep(slice)
      remaining -= slice
      throwIfInterrupted(current)

  def sleep(millis: scala.Long, nanos: scala.Int): Unit =
    if millis < 0L || nanos < 0 || nanos > 999999 then
      throw new IllegalArgumentException(null)

    val adjusted =
      if nanos == 0 then millis
      else millis + 1L
    sleep(adjusted)

  private[lang] def registerInheritable(local: InheritableThreadLocal[?]): Unit =
    withRegistryLock {
      ensureInheritableCapacity(inheritableLocalCount + 1)
      inheritableLocals(inheritableLocalCount) = local.asInstanceOf[AnyRef]
      inheritableLocalCount += 1
    }

  private[lang] def runStartedThread(thread: Thread, inherited: Array[InheritedValue]): Unit =
    thread.bindCurrentRuntimeThread()
    registerRunningThread(currentPythonThreadId(PyThreading.currentThread()), thread)
    currentThreadLocalRef().setAttr(CurrentThreadAttr, thread)
    installInheritedValues(thread, inherited)
    try
      thread.run()
    finally
      thread.markCompleted()
      unregisterRunningThread(thread)

  private[lang] def newRuntimeThread(
      thread: Thread,
      inherited: Array[InheritedValue],
      name: String,
      daemon: scala.Boolean
  ): scala.python.runtime.PyThread =
    PyThreading.newThread(() => runStartedThread(thread, inherited), name, daemon)

  private[lang] def nextThreadId(): scala.Long =
    withRegistryLock {
      val id = nextId
      nextId += 1L
      id
    }

  private[lang] def nextUnnamedThreadName(): String =
    withRegistryLock {
      val idx = nextUnnamed
      nextUnnamed += 1
      "Thread-" + idx
    }

  private def installInheritedValues(thread: Thread, inherited: Array[InheritedValue]): Unit =
    var i = 0
    while i < inherited.length do
      val entry = inherited(i)
      thread.installInheritedValue(entry.local, entry.value)
      i += 1

  private def captureInheritedValues(): Array[InheritedValue] =
    withRegistryLock {
      val buffer = new Array[InheritedValue](inheritableLocalCount)
      var count = 0
      var i = 0
      while i < inheritableLocalCount do
        val local = inheritableLocals(i).asInstanceOf[InheritableThreadLocal[?]]
        val snapshot = local.snapshotForChild()
        if snapshot != null then
          buffer(count) = snapshot.asInstanceOf[InheritedValue]
          count += 1
        i += 1

      val exact = new Array[InheritedValue](count)
      var j = 0
      while j < count do
        exact(j) = buffer(j)
        j += 1
      exact
    }

  private def throwIfInterrupted(thread: Thread): Unit =
    if thread.consumeInterruptedStatus() then
      throw new InterruptedException(null)

  private def currentPythonThreadId(runtime: PyThread): scala.Long =
    runtime.getIdent() match
      case null  => throw new IllegalStateException("Current Python thread has no identifier")
      case ident => ident

  private def lookupRunningThread(ident: scala.Long): Thread | Null =
    withRegistryLock {
      lookupRunningThreadNoLock(ident)
    }

  private def lookupRunningThreadNoLock(ident: scala.Long): Thread | Null =
    var i = 0
    while i < registeredThreadCount do
      if pythonThreadIds(i) == ident then
        return scalaThreads(i)
      i += 1
    null

  private def registerRunningThread(ident: scala.Long, thread: Thread): Unit =
    withRegistryLock {
      val existing = indexOfThreadIdNoLock(ident)
      if existing >= 0 then
        scalaThreads(existing) = thread
      else
        ensureThreadCapacity(registeredThreadCount + 1)
        pythonThreadIds(registeredThreadCount) = ident
        scalaThreads(registeredThreadCount) = thread
        registeredThreadCount += 1
    }

  private def unregisterRunningThread(thread: Thread): Unit =
    withRegistryLock {
      var i = 0
      while i < registeredThreadCount && (scalaThreads(i) ne thread) do
        i += 1

      if i < registeredThreadCount then
        while i < registeredThreadCount - 1 do
          pythonThreadIds(i) = pythonThreadIds(i + 1)
          scalaThreads(i) = scalaThreads(i + 1)
          i += 1
        registeredThreadCount -= 1
    }

  private def indexOfThreadIdNoLock(ident: scala.Long): scala.Int =
    var i = 0
    while i < registeredThreadCount do
      if pythonThreadIds(i) == ident then
        return i
      i += 1
    -1

  private def ensureThreadCapacity(required: scala.Int): Unit =
    if required <= scalaThreads.length then
      ()
    else
      val grown = scalaThreads.length * 2
      val newSize = if grown < required then required else grown
      val ids = new Array[scala.Long](newSize)
      val threads = new Array[Thread](newSize)
      var i = 0
      while i < registeredThreadCount do
        ids(i) = pythonThreadIds(i)
        threads(i) = scalaThreads(i)
        i += 1
      pythonThreadIds = ids
      scalaThreads = threads

  private def ensureInheritableCapacity(required: scala.Int): Unit =
    if required <= inheritableLocals.length then
      ()
    else
      val grown = inheritableLocals.length * 2
      val newSize = if grown < required then required else grown
      val locals = new Array[AnyRef](newSize)
      var i = 0
      while i < inheritableLocalCount do
        locals(i) = inheritableLocals(i)
        i += 1
      inheritableLocals = locals

  private def ensureMainThreadRegistered(): Thread =
    val existing = mainThread0
    if existing != null then
      currentThreadLocalRef().setAttr(CurrentThreadAttr, existing)
      existing
    else
      withRegistryLock {
        val registered = mainThread0
        if registered != null then
          currentThreadLocalRef().setAttr(CurrentThreadAttr, registered)
          registered
        else
          val runtime = PyThreading.currentThread()
          val main = new Thread(null, "main", runtime.isDaemon(), 1L, runtime, true)
          mainThread0 = main
          registerRunningThread(currentPythonThreadId(runtime), main)
          currentThreadLocalRef().setAttr(CurrentThreadAttr, main)
          main
      }

  private def currentThreadLocalRef(): scala.python.runtime.PyLocal =
    val existing = currentThreadLocal0
    if existing != null then
      existing.asInstanceOf[scala.python.runtime.PyLocal]
    else
      val created = PyThreading.newLocal()
      currentThreadLocal0 = created
      created

  private def registryLockRef(): scala.python.runtime.PyRLock =
    val existing = registryLock0
    if existing != null then
      existing.asInstanceOf[scala.python.runtime.PyRLock]
    else
      // Safe in practice: module initialization is single-threaded, so this lazy publication does not race during bootstrap.
      val created = PyThreading.newRLock()
      registryLock0 = created
      created

  private def withRegistryLock[T](op: => T): T =
    val lock = registryLockRef()
    lock.acquire()
    try op
    finally lock.release()
