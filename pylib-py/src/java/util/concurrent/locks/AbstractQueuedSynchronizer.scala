package java.util.concurrent.locks

import java.io.Serializable
import java.lang.{InterruptedException, System, Thread}
import java.util.concurrent.TimeUnit

import scala.python.runtime.{PyRLock, PyCondition, PyThreading}

/** A `java.util.concurrent.locks.AbstractQueuedSynchronizer` (AQS) port
 *  for the Python backend, backed by a real `threading.Condition`.
 *
 *  AQS is a JVM-internal kit for building synchronizers (locks,
 *  latches, semaphores). Subclasses override `tryAcquireShared` /
 *  `tryReleaseShared` (or the exclusive counterparts) and call into
 *  the framework via `acquireShared`, `releaseShared`,
 *  `acquireSharedInterruptibly`, `tryAcquireSharedNanos`, etc. The
 *  framework owns the wait queue.
 *
 *  This port is NOT a re-implementation of the JVM CLH wait queue. It
 *  is a `threading.Condition`-backed adapter:
 *
 *    - `getState` / `setState` / `compareAndSetState` track the state
 *      field, guarded by `stateLock`.
 *    - `acquire`, `acquireShared`, `acquireSharedInterruptibly`,
 *      `tryAcquireSharedNanos`, etc. wait on the condition variable
 *      until either `tryAcquireShared(arg) >= 0` (or the exclusive
 *      counterpart succeeds) or interruption / timeout occurs.
 *    - `release`, `releaseShared` invoke the user override and on
 *      success call `notify_all` on the condition so that all blocked
 *      threads re-evaluate their guard.
 *
 *  This implementation is correct under genuine multi-thread
 *  contention: a `releaseShared` from one thread is observed by
 *  another thread blocked in `acquireSharedInterruptibly` because the
 *  release path acquires the same `stateLock` and signals the
 *  condition. It is intentionally simpler than the JVM CLH queue —
 *  fairness and queue-ordering guarantees are weaker (Python's
 *  `Condition.notify_all` wakes all waiters, which compete for the
 *  state lock; this is best-effort FIFO under CPython, similar to
 *  pthread mutex/condvar contention).
 *
 *  The Wave 5 item 06 single-threaded busy-poll shell (which only
 *  worked because emitted Scala code was single-threaded under the
 *  GIL) has been removed in favour of this port. See
 *  `notes/wave6-worklist/09-jvm-concurrent-surface.md`.
 */
abstract class AbstractQueuedSynchronizer extends AbstractOwnableSynchronizer with Serializable:
  import AbstractQueuedSynchronizer.*

  // The state lock guards `state0` and serves as the underlying mutex
  // of `stateCond`. We use an `RLock` so that a `tryRelease` callback
  // (invoked while we hold the lock) can re-enter without deadlocking
  // if a subclass's override happens to call back into framework
  // methods that also need the lock.
  private val stateLock: PyRLock = PyThreading.newRLock()
  private val stateCond: PyCondition = PyThreading.newCondition(stateLock)

  @volatile private var state0: Int = 0

  protected final def getState(): Int =
    stateLock.acquire()
    try state0
    finally stateLock.release()

  protected final def setState(newState: Int): Unit =
    stateLock.acquire()
    try state0 = newState
    finally stateLock.release()

  protected final def compareAndSetState(expect: Int, update: Int): Boolean =
    stateLock.acquire()
    try
      if state0 == expect then
        state0 = update
        true
      else false
    finally stateLock.release()

  // --- Methods subclasses are expected to override --------------------

  protected def tryAcquire(arg: Int): Boolean =
    throw new UnsupportedOperationException()

  protected def tryRelease(arg: Int): Boolean =
    throw new UnsupportedOperationException()

  protected def tryAcquireShared(arg: Int): Int =
    throw new UnsupportedOperationException()

  protected def tryReleaseShared(arg: Int): Boolean =
    throw new UnsupportedOperationException()

  protected def isHeldExclusively(): Boolean =
    throw new UnsupportedOperationException()

  // --- Public acquire/release entry points -----------------------------

  final def acquire(arg: Int): Unit =
    if tryAcquire(arg) then return
    stateLock.acquire()
    try
      while !tryAcquire(arg) do
        // `wait` releases stateLock atomically, then re-acquires on
        // wake-up. A real `threading.Condition`-backed wait, not a
        // busy-spin.
        stateCond.waitReady()
    finally stateLock.release()

  final def acquireInterruptibly(arg: Int): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquire(arg) then return
    stateLock.acquire()
    try
      while !tryAcquire(arg) do
        // Wait with a small timeout so we can poll the interrupt flag.
        stateCond.waitReady(InterruptPollMillis)
        if Thread.interrupted() then
          throw new InterruptedException(null)
    finally stateLock.release()

  final def tryAcquireNanos(arg: Int, nanosTimeout: Long): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquire(arg) then return true
    if nanosTimeout <= 0L then return false
    val deadline = System.nanoTime() + nanosTimeout
    stateLock.acquire()
    try
      var result = false
      var done = false
      while !done do
        if tryAcquire(arg) then
          result = true
          done = true
        else
          val remaining = deadline - System.nanoTime()
          if remaining <= 0L then
            done = true
          else
            val sliceMillis = clampMillisFromNanos(remaining)
            stateCond.waitReady(sliceMillis)
            if Thread.interrupted() then
              throw new InterruptedException(null)
      result
    finally stateLock.release()

  final def release(arg: Int): Boolean =
    stateLock.acquire()
    try
      if tryRelease(arg) then
        // Wake all waiters; each re-evaluates its acquire guard. JVM
        // AQS only wakes the queue head; this is more conservative
        // (no waiter starvation) at the cost of thundering-herd wake-
        // ups. For the Promise/CompletionLatch consumer the difference
        // is invisible.
        stateCond.notifyAllThreads()
        true
      else false
    finally stateLock.release()

  final def acquireShared(arg: Int): Unit =
    if tryAcquireShared(arg) >= 0 then return
    stateLock.acquire()
    try
      while tryAcquireShared(arg) < 0 do
        stateCond.waitReady()
    finally stateLock.release()

  final def acquireSharedInterruptibly(arg: Int): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquireShared(arg) >= 0 then return
    stateLock.acquire()
    try
      while tryAcquireShared(arg) < 0 do
        stateCond.waitReady(InterruptPollMillis)
        if Thread.interrupted() then
          throw new InterruptedException(null)
    finally stateLock.release()

  final def tryAcquireSharedNanos(arg: Int, nanosTimeout: Long): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquireShared(arg) >= 0 then return true
    if nanosTimeout <= 0L then return false
    val deadline = System.nanoTime() + nanosTimeout
    stateLock.acquire()
    try
      var result = false
      var done = false
      while !done do
        if tryAcquireShared(arg) >= 0 then
          result = true
          done = true
        else
          val remaining = deadline - System.nanoTime()
          if remaining <= 0L then
            done = true
          else
            val sliceMillis = clampMillisFromNanos(remaining)
            stateCond.waitReady(sliceMillis)
            if Thread.interrupted() then
              throw new InterruptedException(null)
      result
    finally stateLock.release()

  final def releaseShared(arg: Int): Boolean =
    stateLock.acquire()
    try
      if tryReleaseShared(arg) then
        stateCond.notifyAllThreads()
        true
      else false
    finally stateLock.release()

  // The full JVM AQS exposes a queue inspection API. Without an
  // explicit queue we report neutral values; consumers like
  // `CompletionLatch` do not query these.
  final def hasQueuedThreads(): Boolean = false
  final def hasContended(): Boolean = false
  final def getFirstQueuedThread(): Thread | Null = null
  final def isQueued(thread: Thread): Boolean = false
  final def getQueueLength(): Int = 0

  // --- Helpers -------------------------------------------------------

  private def clampMillisFromNanos(nanos: Long): Long =
    val raw = nanos / 1000000L
    if raw <= 0L then 1L
    else if raw > MaxWaitMillis then MaxWaitMillis
    else raw

object AbstractQueuedSynchronizer:
  // Cap individual wait slices so an unmanaged consumer can still
  // interrupt promptly even on platforms with coarse timer resolution.
  final val MaxWaitMillis: Long = 5000L

  // Periodic poll cadence inside the interruptible variants.
  final val InterruptPollMillis: Long = 50L
