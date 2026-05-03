// Real-contention test for `AbstractQueuedSynchronizer`. Two threads
// race to release/acquire a one-shot shared latch built directly on
// AQS — the same shape as `scala.concurrent.impl.CompletionLatch` used
// by `Future.await`. A single-threaded busy-poll shell would either
// burn the CPU or fail to wake the second thread; the
// `threading.Condition`-backed port handles real cross-thread signal
// propagation.

import java.util.concurrent.locks.AbstractQueuedSynchronizer

import scala.python.runtime.PyThreading

private final class OneShotLatch extends AbstractQueuedSynchronizer:
  override protected def tryAcquireShared(arg: Int): Int =
    if getState() != 0 then 1 else -1

  override protected def tryReleaseShared(arg: Int): Boolean =
    setState(1)
    true

@main def javalibConcurrentAqs(): Unit =
  val latch = new OneShotLatch
  val acquireStarted = PyThreading.newEvent()
  var awoke = false

  val waiter = new java.lang.Thread(() =>
    acquireStarted.set()
    latch.acquireSharedInterruptibly(1)
    awoke = true
  )
  waiter.start()
  acquireStarted.waitReady()

  // Give the waiter time to actually enter the wait queue (i.e. block
  // on the condition variable). The lower bound here is well above
  // the AQS poll cadence (~50ms); we sleep 200ms to be safe.
  PyThreading.sleep(200L)
  val beforeRelease = awoke

  // Now signal from this thread; the waiter must observe the change
  // and unblock. A busy-poll shell would have already terminated (it
  // wouldn't actually block), so `beforeRelease` would be `true` and
  // the test trivially "passes" — that's the failure shape we want to
  // avoid.
  latch.releaseShared(1)
  waiter.join()

  println("aqs-shared:" + beforeRelease + ":" + awoke)

  // Timed acquire that succeeds before timeout.
  val timed = new OneShotLatch
  val timedRelease = new java.lang.Thread(() =>
    PyThreading.sleep(50L)
    timed.releaseShared(1)
    ()
  )
  timedRelease.start()
  val timedResult = timed.tryAcquireSharedNanos(1, 5L * 1000L * 1000L * 1000L)
  timedRelease.join()
  println("aqs-timed-success:" + timedResult)

  // Timed acquire that times out (no release).
  val timedOut = new OneShotLatch
  val timedOutResult = timedOut.tryAcquireSharedNanos(1, 50L * 1000L * 1000L)
  println("aqs-timed-timeout:" + timedOutResult)
