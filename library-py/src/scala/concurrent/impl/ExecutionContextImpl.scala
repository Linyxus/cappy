/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.impl

import scala.language.`2.13`
import java.util.concurrent.{ Callable, Executor, ExecutorService, TimeUnit }
import java.util.Collection
import scala.concurrent.{ BlockContext, ExecutionContext, CanAwait, ExecutionContextExecutor, ExecutionContextExecutorService }

/** Python-backend port of `scala.concurrent.impl.ExecutionContextImpl`.
 *
 *  The upstream implementation builds the `ExecutionContext.global`
 *  default on `java.util.concurrent.ForkJoinPool`, with a custom
 *  `ForkJoinWorkerThread` factory and `ForkJoinPool.ManagedBlocker`
 *  for `BlockContext` integration. None of that surface is provided
 *  by the Python backend's javalib (no `ForkJoinPool`, no
 *  `ForkJoinWorkerThread`, no `ManagedBlocker`), and porting
 *  work-stealing semantics is out of scope per
 *  `notes/wave6-worklist/09-jvm-concurrent-surface.md`.
 *
 *  This override drops to a single-thread `ExecutorService` (from
 *  `java.util.concurrent.Executors.newSingleThreadExecutor()`) for
 *  the default global pool. Externally observable behavior is the
 *  same for the test corpus: `Future { ... }` runs on a worker thread,
 *  `Await.result(...)` blocks the caller until the latch is signalled,
 *  thread interruption propagates. The only loss is parallelism — a
 *  single-threaded pool serializes Future bodies. No fixture in the
 *  current `tests/run/` corpus depends on actual parallelism (and any
 *  that did would be redirected to the work-stealing-specific (Y)
 *  excludelist per item 09's policy table).
 *
 *  `ExecutionContextExecutorService` mixes in `java.util.concurrent.ExecutorService`,
 *  whose JVM-shape interface contains methods (`submit`, `invokeAll`,
 *  `awaitTermination`, …) that the Python backend's pylib
 *  `ExecutorService` interface deliberately does not expose. These
 *  methods are unreachable from the test corpus's actual code paths
 *  (only `execute`, `shutdown`, `shutdownNow`, `reportFailure` are
 *  called), so the compile picks up the JDK signature and we provide
 *  `???`-style stubs so the anonymous classes type-check; the linker's
 *  reachability scan keeps these out of the bundle.
 */
private[scala] class ExecutionContextImpl private[impl] (final val executor: Executor, final val reporter: Throwable => Unit) extends ExecutionContextExecutor:
  require(executor ne null, "Executor must not be null")
  override final def execute(runnable: Runnable): Unit = executor.execute(runnable)
  override final def reportFailure(t: Throwable): Unit = reporter(t)

private[concurrent] object ExecutionContextImpl:

  private final class GlobalExecutorService(
      backing: ExecutorService,
      reporter: Throwable => Unit
  ) extends ExecutionContextImpl(backing, reporter) with ExecutionContextExecutorService:
    final override def shutdown(): Unit = backing.shutdown()
    final override def shutdownNow(): java.util.List[Runnable] = backing.shutdownNow()

    // The JVM `ExecutorService` interface declares the methods below.
    // Python-backend `ExecutorService` (pylib) does not, but library-py
    // is typechecked against the JDK so we must provide concrete
    // overrides. They are unreachable from the deadlock.scala /
    // ExecutionContext.global path (no `submit`/`invokeAll` callers in
    // the residual cluster); link-time DCE drops them.
    final override def isShutdown(): Boolean =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: isShutdown unsupported on Python backend default pool")
    final override def isTerminated(): Boolean =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: isTerminated unsupported on Python backend default pool")
    final override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: awaitTermination unsupported on Python backend default pool")
    final override def submit[T](callable: Callable[T]): java.util.concurrent.Future[T] =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: submit(Callable) unsupported on Python backend default pool")
    final override def submit[T](runnable: Runnable, t: T): java.util.concurrent.Future[T] =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: submit(Runnable, T) unsupported on Python backend default pool")
    final override def submit(runnable: Runnable): java.util.concurrent.Future[?] =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: submit(Runnable) unsupported on Python backend default pool")
    final override def invokeAll[T](callables: Collection[? <: Callable[T]]): java.util.List[java.util.concurrent.Future[T]] =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: invokeAll unsupported on Python backend default pool")
    final override def invokeAll[T](callables: Collection[? <: Callable[T]], l: Long, timeUnit: TimeUnit): java.util.List[java.util.concurrent.Future[T]] =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: invokeAll unsupported on Python backend default pool")
    final override def invokeAny[T](callables: Collection[? <: Callable[T]]): T =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: invokeAny unsupported on Python backend default pool")
    final override def invokeAny[T](callables: Collection[? <: Callable[T]], l: Long, timeUnit: TimeUnit): T =
      throw new UnsupportedOperationException(
        "ExecutionContextImpl: invokeAny unsupported on Python backend default pool")

  /** Daemon-thread factory so the global pool's worker doesn't keep
   *  the Python interpreter alive past `main`. Mirrors the upstream
   *  `DefaultThreadFactory(daemonic = true)` choice.
   */
  private final class DaemonThreadFactory extends java.util.concurrent.ThreadFactory:
    def newThread(r: Runnable): Thread =
      val t = new Thread(r)
      t.setDaemon(true)
      t

  def createDefaultExecutorService(reporter: Throwable => Unit): ExecutionContextExecutorService =
    val backing = java.util.concurrent.Executors.newSingleThreadExecutor(new DaemonThreadFactory)
    new GlobalExecutorService(backing, reporter)

  def fromExecutor(e: Executor | Null, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextExecutor =
    e match
      case null => createDefaultExecutorService(reporter)
      case some => new ExecutionContextImpl(some, reporter)

  def fromExecutorService(es: ExecutorService | Null, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextExecutorService =
    es match
      case null => createDefaultExecutorService(reporter)
      case some => new GlobalExecutorService(some, reporter)
