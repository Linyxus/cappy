package java.util.concurrent

import java.util.List

trait ExecutorService extends Executor:
  def shutdown(): Unit
  def shutdownNow(): List[Runnable]
