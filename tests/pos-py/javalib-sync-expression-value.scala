final class SyncLockValue

@main def javalibSyncExpressionValue(): Unit =
  val lock = new SyncLockValue()
  val value = lock.synchronized {
    40 + 2
  }
  println("value:" + value)
