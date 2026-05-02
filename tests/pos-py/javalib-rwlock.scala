import java.util.concurrent.locks.ReentrantReadWriteLock

@main def javalibRwlock(): Unit =
  val rwl = new ReentrantReadWriteLock()
  val rlock = rwl.readLock()
  val wlock = rwl.writeLock()

  // Basic write-lock + unlock
  wlock.lock()
  val writeHeld = rwl.isWriteLockedByCurrentThread()
  wlock.unlock()
  println("write-basic:" + writeHeld + ":" + rwl.isWriteLocked())

  // Read lock allows multiple acquires from same thread
  rlock.lock()
  rlock.lock()
  val readCount = rwl.getReadLockCount()
  rlock.unlock()
  rlock.unlock()
  println("read-reentrant:" + readCount + ":" + rwl.getReadLockCount())

  // Re-entrant write
  wlock.lock()
  wlock.lock()
  val writeHold = rwl.getWriteHoldCount()
  wlock.unlock()
  wlock.unlock()
  println("write-reentrant:" + writeHold + ":" + rwl.getWriteHoldCount())

  // Writer can also acquire read lock (downgrade pattern)
  wlock.lock()
  rlock.lock()
  val downgradeRead = rwl.getReadLockCount()
  val downgradeWrite = rwl.isWriteLockedByCurrentThread()
  rlock.unlock()
  wlock.unlock()
  println("downgrade:" + downgradeRead + ":" + downgradeWrite)

  // tryLock should succeed when free
  val tryR = rlock.tryLock()
  if tryR then rlock.unlock()
  val tryW = wlock.tryLock()
  if tryW then wlock.unlock()
  println("trylock:" + tryR + ":" + tryW)

  // Use Symbol which internally relies on ReentrantReadWriteLock.
  val s1 = Symbol("hello")
  val s2 = Symbol("hello")
  val s3 = Symbol("world")
  println("symbol:" + (s1 eq s2) + ":" + (s1 eq s3) + ":" + s1.name)
