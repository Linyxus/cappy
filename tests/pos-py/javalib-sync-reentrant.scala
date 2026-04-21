final class SyncLockReentrant

@main def javalibSyncReentrant(): Unit =
  val lock = new SyncLockReentrant()
  var value = 0
  lock.synchronized {
    value += 1
    lock.synchronized {
      value += 1
    }
  }
  println("reentrant:" + value)
