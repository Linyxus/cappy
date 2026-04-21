import java.lang.Thread
import java.util.ArrayList
import java.util.concurrent.*

private def inSubmissionOrder(values: ArrayList[Int], expectedSize: Int): Boolean =
  if values.size() != expectedSize then
    false
  else
    var index = 0
    while index < expectedSize do
      if values.get(index) != index then
        return false
      index += 1
    true

@main def javalibConcurrentExecutors(): Unit =
  val factory1 = Executors.defaultThreadFactory()
  val factory1Thread1 = factory1.newThread(() => ())
  val factory1Thread2 = factory1.newThread(() => ())
  val factory2 = Executors.defaultThreadFactory()
  val factory2Thread1 = factory2.newThread(() => ())
  println(
    "default-factory:" +
      factory1Thread1.getName() + ":" +
      factory1Thread2.getName() + ":" +
      factory2Thread1.getName()
  )

  val done = new CountDownLatch(100)
  val values = new ArrayList[Int]()
  val workerNames = new ConcurrentSkipListSet[String]()
  val executor = Executors.newSingleThreadExecutor()

  var next = 0
  while next < 100 do
    val taskValue = next
    executor.execute(() =>
      workerNames.add(Thread.currentThread().getName())
      values.add(taskValue)
      done.countDown()
    )
    next += 1

  done.await()
  executor.shutdown()

  println(
    "single-thread-executor:" +
      values.size() + ":" +
      inSubmissionOrder(values, 100) + ":" +
      workerNames.size() + ":" +
      workerNames.first()
  )

  try
    executor.execute(() => ())
    println("executor-shutdown:fail")
  catch
    case _: RejectedExecutionException =>
      println("executor-shutdown:true")
