package java.util.concurrent.locks

import java.io.Serializable
import java.lang.Thread

/** Single-threaded port of `AbstractOwnableSynchronizer`. */
abstract class AbstractOwnableSynchronizer protected () extends Serializable:
  private var exclusiveOwner: Thread | Null = null

  protected final def setExclusiveOwnerThread(thread: Thread | Null): Unit =
    exclusiveOwner = thread

  protected final def getExclusiveOwnerThread(): Thread | Null =
    exclusiveOwner
