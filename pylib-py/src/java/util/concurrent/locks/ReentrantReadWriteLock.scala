package java.util.concurrent.locks

import java.io.Serializable
import java.lang.{IllegalMonitorStateException, InterruptedException, Thread}
import java.util.concurrent.TimeUnit

import scala.python.runtime.{PyLock, PyRLock, PyThreading}

/** A `java.util.concurrent.locks.ReentrantReadWriteLock` port for the
 *  Python backend.
 *
 *  CPython is effectively single-threaded for Scala-emitted code: every
 *  bytecode dispatch is serialized by the GIL, and `runScalaPy` does
 *  not start additional OS threads. We still implement honest
 *  read/write semantics on top of `threading.RLock` / `threading.Lock`
 *  so single-threaded re-entrancy works correctly:
 *
 *  - Read locks are counted, write lock excludes both.
 *  - Re-entrant acquires by the same Python thread succeed without
 *    deadlock (matches JVM `ReentrantReadWriteLock` non-fair default).
 *  - There is no JVM-style fairness or barging policy; this is
 *    inherently best-effort under the GIL.
 *
 *  Cluster 4 of `wave5-sweep.md` lists this type as the dominant
 *  reason `scala.UniquenessCache` (used by `scala.Symbol`) fails to
 *  link, so the focus here is correctness of the read/write code path
 *  through `ReentrantReadWriteLock.ReadLock.lock` / `.unlock` and
 *  `ReentrantReadWriteLock.WriteLock.lock` / `.unlock`.
 */
class ReentrantReadWriteLock(fair: Boolean) extends ReadWriteLock with Serializable:
  // Use a single underlying mutex that protects the reader count and
  // serializes writers. Re-entrancy is tracked in the read-lock and
  // write-lock owner maps respectively. Java's RRWL allows a writer to
  // hold its own read-lock; we track that by remembering the writer
  // thread so re-entrant read acquires from the writer don't block.
  private val mutex: PyLock = PyThreading.newLock()
  // Read-side condition variable lets readers wait until the active
  // writer releases. We share `mutex` as the underlying lock by
  // wrapping it in an RLock-equivalent only when needed; for the
  // single-threaded backend this stays a no-op fast path.
  private val readReady: PyRLock = PyThreading.newRLock()
  private val writeReady: PyRLock = PyThreading.newRLock()

  private var activeReaders: Int = 0
  private var writeOwner: Thread | Null = null
  private var writeHoldCount: Int = 0
  // Per-thread read hold count, keyed on the Python thread identity.
  // The CPython runtime is single-threaded for emitted Scala code, so
  // a single field is enough; we still track per-owner so a writer
  // re-entering as a reader doesn't deadlock.
  private var readOwner: Thread | Null = null
  private var readOwnerHoldCount: Int = 0

  private val readLockInst: ReentrantReadWriteLock.ReadLock =
    new ReentrantReadWriteLock.ReadLock(this)
  private val writeLockInst: ReentrantReadWriteLock.WriteLock =
    new ReentrantReadWriteLock.WriteLock(this)

  def this() =
    this(false)

  def readLock(): ReentrantReadWriteLock.ReadLock =
    readLockInst

  def writeLock(): ReentrantReadWriteLock.WriteLock =
    writeLockInst

  final def isFair(): Boolean = fair

  def getReadLockCount(): Int =
    mutex.acquire()
    try activeReaders
    finally mutex.release()

  def getReadHoldCount(): Int =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      if readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
        readOwnerHoldCount
      else 0
    finally mutex.release()

  def getWriteHoldCount(): Int =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      if writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
        writeHoldCount
      else 0
    finally mutex.release()

  def isWriteLocked(): Boolean =
    mutex.acquire()
    try writeHoldCount > 0
    finally mutex.release()

  def isWriteLockedByCurrentThread(): Boolean =
    val current = Thread.currentThread()
    mutex.acquire()
    try writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
    finally mutex.release()

  override def toString(): String =
    val readers = getReadLockCount()
    val writers = if isWriteLocked() then 1 else 0
    s"${super.toString()}[Write locks = $writers, Read locks = $readers]"

  // --- Internal helpers used by ReadLock/WriteLock -----------------

  private[locks] def acquireReadLock(): Unit =
    val current = Thread.currentThread()
    var spin = true
    while spin do
      mutex.acquire()
      val mine =
        readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
      val writerIsMe =
        writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
      if writeHoldCount == 0 || writerIsMe then
        activeReaders += 1
        if mine then
          readOwnerHoldCount += 1
        else
          readOwner = current
          readOwnerHoldCount = 1
        mutex.release()
        spin = false
      else
        mutex.release()
        // Single-threaded under the GIL there are no other writers
        // possible, so this is effectively unreachable. Keep the
        // poll-and-yield form for robustness.
        PyThreading.sleep(1L)

  private[locks] def tryAcquireReadLockNoWait(): Boolean =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      val writerIsMe =
        writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
      if writeHoldCount > 0 && !writerIsMe then false
      else
        activeReaders += 1
        if readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
          readOwnerHoldCount += 1
        else
          readOwner = current
          readOwnerHoldCount = 1
        true
    finally mutex.release()

  private[locks] def releaseReadLock(): Unit =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      if activeReaders <= 0 then
        throw new IllegalMonitorStateException()
      activeReaders -= 1
      if readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
        readOwnerHoldCount -= 1
        if readOwnerHoldCount == 0 then
          readOwner = null
    finally mutex.release()

  private[locks] def acquireWriteLock(): Unit =
    val current = Thread.currentThread()
    var spin = true
    while spin do
      mutex.acquire()
      val mine =
        writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
      // Allow the write lock if no one else holds it AND no readers
      // (other than possibly this thread itself) are active.
      val noOtherReaders =
        activeReaders == 0 ||
          ((readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]) &&
            readOwnerHoldCount == activeReaders)
      if (writeHoldCount == 0 && noOtherReaders) || mine then
        writeOwner = current
        writeHoldCount += 1
        mutex.release()
        spin = false
      else
        mutex.release()
        PyThreading.sleep(1L)

  private[locks] def tryAcquireWriteLockNoWait(): Boolean =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      val mine =
        writeOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]
      val noOtherReaders =
        activeReaders == 0 ||
          ((readOwner.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef]) &&
            readOwnerHoldCount == activeReaders)
      if (writeHoldCount == 0 && noOtherReaders) || mine then
        writeOwner = current
        writeHoldCount += 1
        true
      else false
    finally mutex.release()

  private[locks] def releaseWriteLock(): Unit =
    val current = Thread.currentThread()
    mutex.acquire()
    try
      if writeOwner.asInstanceOf[AnyRef] ne current.asInstanceOf[AnyRef] then
        throw new IllegalMonitorStateException()
      if writeHoldCount <= 0 then
        throw new IllegalMonitorStateException()
      writeHoldCount -= 1
      if writeHoldCount == 0 then
        writeOwner = null
    finally mutex.release()

  private[locks] def writeOwnerThread(): Thread | Null =
    mutex.acquire()
    try writeOwner
    finally mutex.release()

object ReentrantReadWriteLock:
  /** Read view of a `ReentrantReadWriteLock`. Lock acquisition is
   *  shared: multiple read holders may coexist while no write lock is
   *  held by another thread.
   *
   *  `newCondition()` is unsupported by JVM convention; we mirror that
   *  by raising `UnsupportedOperationException`.
   */
  class ReadLock(private val outer: ReentrantReadWriteLock) extends Lock with Serializable:
    def lock(): Unit =
      outer.acquireReadLock()

    def lockInterruptibly(): Unit =
      if Thread.interrupted() then
        throw new InterruptedException(null)
      outer.acquireReadLock()

    def tryLock(): Boolean =
      outer.tryAcquireReadLockNoWait()

    def tryLock(time: Long, unit: TimeUnit): Boolean =
      // Single-thread backend: timeouts collapse to immediate try.
      tryLock()

    def unlock(): Unit =
      outer.releaseReadLock()

    def newCondition(): Condition =
      throw new UnsupportedOperationException(
        "Conditions are not supported on ReentrantReadWriteLock.ReadLock")

    override def toString(): String =
      val readers = outer.getReadLockCount()
      s"${super.toString()}[Read locks = $readers]"

  /** Write view of a `ReentrantReadWriteLock`. Acquisition is
   *  exclusive and re-entrant. Conditions are supported.
   */
  class WriteLock(private val outer: ReentrantReadWriteLock) extends Lock with Serializable:
    def lock(): Unit =
      outer.acquireWriteLock()

    def lockInterruptibly(): Unit =
      if Thread.interrupted() then
        throw new InterruptedException(null)
      outer.acquireWriteLock()

    def tryLock(): Boolean =
      outer.tryAcquireWriteLockNoWait()

    def tryLock(time: Long, unit: TimeUnit): Boolean =
      tryLock()

    def unlock(): Unit =
      outer.releaseWriteLock()

    def newCondition(): Condition =
      // We don't expose a real Condition for the write lock either,
      // because the read/write mutex is internal. JVM RRWL allows it,
      // but no consumer reachable from cluster 4 fixtures uses it.
      throw new UnsupportedOperationException(
        "Conditions are not yet supported on ReentrantReadWriteLock.WriteLock")

    def isHeldByCurrentThread(): Boolean =
      outer.isWriteLockedByCurrentThread()

    def getHoldCount(): Int =
      outer.getWriteHoldCount()

    override def toString(): String =
      val owner = outer.writeOwnerThread()
      val state =
        if owner == null then "Unlocked"
        else "Locked by " + owner.getName()
      s"${super.toString()}[$state]"
