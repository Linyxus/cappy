package java.util.concurrent.locks

import java.io.Serializable
import java.lang.{InterruptedException, System, Thread}
import java.util.concurrent.TimeUnit

import scala.python.runtime.PyThreading

/** Single-threaded port of `AbstractQueuedSynchronizer` (AQS).
 *
 *  AQS is a JVM-internal kit for building synchronizers (locks,
 *  latches, semaphores). Subclasses override `tryAcquireShared` /
 *  `tryReleaseShared` (or the exclusive counterparts) and call into
 *  the framework via `acquireShared`, `releaseShared`,
 *  `acquireSharedInterruptibly`, `tryAcquireSharedNanos`, etc. The
 *  framework owns the wait queue.
 *
 *  Under CPython, all Scala-emitted code is serialized by the GIL and
 *  `runScalaPy` does not start additional OS threads. The reachable
 *  AQS consumer in Wave 5 fixtures is
 *  `scala.concurrent.impl.CompletionLatch`, used by
 *  `Future.tryAwait0` to block until a Promise completes. In the
 *  single-threaded backend, callbacks attached via `onComplete(...)`
 *  (under `ExecutionContext.parasitic`) execute synchronously before
 *  the await call returns, so by the time `acquireSharedInterruptibly`
 *  / `tryAcquireSharedNanos` runs, `tryAcquireShared` has already
 *  observed a non-zero state and the await returns immediately.
 *
 *  Therefore this is intentionally a *thin* implementation:
 *    - `getState` / `setState` / `compareAndSetState` track the
 *      single integer state field, exactly as in the JVM.
 *    - `releaseShared` and `release` invoke the user override and
 *      treat success as a notification.
 *    - `acquireSharedInterruptibly`, `acquireShared`,
 *      `tryAcquireSharedNanos` poll `tryAcquireShared` and, when
 *      necessary, `Thread.sleep` between polls. Because emitted code
 *      is single-threaded, the first call already succeeds in
 *      practice; the polling fallback is correctness insurance for
 *      unanticipated multi-thread fixtures.
 *
 *  This is documented as "single-threaded honest" rather than a full
 *  AQS port; a complete port would require implementing the
 *  CLH-variant wait queue, which is out of scope for the Python
 *  backend. See `notes/wave5-worklist/06-locks-inventory-and-port.md`.
 */
abstract class AbstractQueuedSynchronizer extends AbstractOwnableSynchronizer with Serializable:
  @volatile private var state0: Int = 0

  protected final def getState(): Int = state0

  protected final def setState(newState: Int): Unit =
    state0 = newState

  protected final def compareAndSetState(expect: Int, update: Int): Boolean =
    if state0 == expect then
      state0 = update
      true
    else false

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
    while !tryAcquire(arg) do
      // CPython is single-threaded so a busy-poll is acceptable as a
      // fallback. In practice the first iteration succeeds.
      PyThreading.sleep(0L)

  final def acquireInterruptibly(arg: Int): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    while !tryAcquire(arg) do
      if Thread.interrupted() then
        throw new InterruptedException(null)
      PyThreading.sleep(0L)

  final def tryAcquireNanos(arg: Int, nanosTimeout: Long): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquire(arg) then return true
    if nanosTimeout <= 0L then return false
    val deadline = System.nanoTime() + nanosTimeout
    while true do
      if tryAcquire(arg) then return true
      if Thread.interrupted() then
        throw new InterruptedException(null)
      val remaining = deadline - System.nanoTime()
      if remaining <= 0L then return false
      val sleepMillis = math.min(remaining / 1000000L, 5L)
      PyThreading.sleep(sleepMillis)
    false

  final def release(arg: Int): Boolean =
    if tryRelease(arg) then true
    else false

  final def acquireShared(arg: Int): Unit =
    while tryAcquireShared(arg) < 0 do
      PyThreading.sleep(0L)

  final def acquireSharedInterruptibly(arg: Int): Unit =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    while tryAcquireShared(arg) < 0 do
      if Thread.interrupted() then
        throw new InterruptedException(null)
      PyThreading.sleep(0L)

  final def tryAcquireSharedNanos(arg: Int, nanosTimeout: Long): Boolean =
    if Thread.interrupted() then
      throw new InterruptedException(null)
    if tryAcquireShared(arg) >= 0 then return true
    if nanosTimeout <= 0L then return false
    val deadline = System.nanoTime() + nanosTimeout
    while true do
      if tryAcquireShared(arg) >= 0 then return true
      if Thread.interrupted() then
        throw new InterruptedException(null)
      val remaining = deadline - System.nanoTime()
      if remaining <= 0L then return false
      val sleepMillis = math.min(remaining / 1000000L, 5L)
      PyThreading.sleep(sleepMillis)
    false

  final def releaseShared(arg: Int): Boolean =
    if tryReleaseShared(arg) then true
    else false

  // The full JVM AQS exposes a queue inspection API; for the
  // single-threaded port these are inert.
  final def hasQueuedThreads(): Boolean = false
  final def hasContended(): Boolean = false
  final def getFirstQueuedThread(): Thread | Null = null
  final def isQueued(thread: Thread): Boolean = false
  final def getQueueLength(): Int = 0
