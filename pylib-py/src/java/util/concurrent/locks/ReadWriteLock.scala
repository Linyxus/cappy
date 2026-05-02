package java.util.concurrent.locks

/** Java's `ReadWriteLock` interface. */
trait ReadWriteLock:
  def readLock(): Lock
  def writeLock(): Lock
