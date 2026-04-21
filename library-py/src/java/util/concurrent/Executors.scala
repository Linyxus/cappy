package java.util.concurrent

import java.util.{ArrayList, List}
import java.util.Objects.requireNonNull

import scala.python.runtime.PyThreading

object Executors:
  private var nextPoolId0 = 1

  private final class DefaultThreadFactory(poolId: Int) extends ThreadFactory:
    private var nextThreadId0 = 1

    def newThread(r: Runnable): Thread =
      val threadNumber =
        this.synchronized {
          val current = nextThreadId0
          nextThreadId0 += 1
          current
        }
      new Thread(r, s"pool-$poolId-thread-$threadNumber")

  private final class SingleThreadExecutor(factory: ThreadFactory) extends ExecutorService:
    private val queue = new ConcurrentLinkedQueue[Runnable]()
    private val stateLock = PyThreading.newRLock()
    private val queueReady = PyThreading.newCondition(stateLock)
    private var accepting = true
    private var stoppingNow = false
    private val worker = factory.newThread(() => runLoop())

    worker.start()

    def execute(command: Runnable): Unit =
      val task = requireNonNull(command)
      stateLock.acquire()
      try
        if !accepting then
          throw new RejectedExecutionException("executor already shut down")
        queue.offer(task)
        queueReady.notifyAllThreads()
      finally stateLock.release()

    def shutdown(): Unit =
      stateLock.acquire()
      try
        accepting = false
        queueReady.notifyAllThreads()
      finally stateLock.release()

    def shutdownNow(): List[Runnable] =
      val pending = new ArrayList[Runnable]()
      stateLock.acquire()
      try
        accepting = false
        stoppingNow = true
        var task = queue.poll()
        while task != null do
          pending.add(task)
          task = queue.poll()
        queueReady.notifyAllThreads()
      finally stateLock.release()
      worker.interrupt()
      pending

    private def runLoop(): Unit =
      var task = awaitNextTask()
      while task != null do
        task.run()
        task = awaitNextTask()

    private def awaitNextTask(): Runnable =
      stateLock.acquire()
      try
        var task = queue.poll()
        while task == null && accepting && !stoppingNow do
          if java.lang.Thread.interrupted() then
            stoppingNow = true
          else
            queueReady.waitReady()
          task = queue.poll()

        if stoppingNow then
          null.asInstanceOf[Runnable]
        else
          task
      finally stateLock.release()

  private def nextPoolId(): Int =
    this.synchronized {
      val current = nextPoolId0
      nextPoolId0 += 1
      current
    }

  def defaultThreadFactory(): ThreadFactory =
    new DefaultThreadFactory(nextPoolId())

  def newSingleThreadExecutor(): ExecutorService =
    new SingleThreadExecutor(defaultThreadFactory())

  def newSingleThreadExecutor(threadFactory: ThreadFactory): ExecutorService =
    new SingleThreadExecutor(threadFactory)
