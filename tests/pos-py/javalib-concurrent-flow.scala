import java.util.concurrent.Flow
import java.util.ArrayList

private final class RecordingSubscriber extends Flow.Subscriber[Int]:
  private var requested = 0L
  private val seen = new ArrayList[Int]()
  private var completed = false

  def onSubscribe(subscription: Flow.Subscription): Unit =
    requested = 3L
    subscription.request(requested)

  def onNext(item: Int): Unit =
    seen.add(item)

  def onError(throwable: Throwable): Unit =
    seen.clear()
    seen.add(-1)

  def onComplete(): Unit =
    completed = true

  def snapshot(): String =
    requested.toString + ":" + seen.toString() + ":" + completed

private final class SimpleSubscription(
    subscriber: RecordingSubscriber,
    values: Array[Int]
) extends Flow.Subscription:
  private var index = 0
  private var cancelled = false

  def request(n: Long): Unit =
    var remaining = n
    while !cancelled && remaining > 0L && index < values.length do
      subscriber.onNext(values(index))
      index += 1
      remaining -= 1L
    if !cancelled && index == values.length then
      subscriber.onComplete()

  def cancel(): Unit =
    cancelled = true

private final class SimplePublisher(values: Array[Int]) extends Flow.Publisher[Int]:
  def subscribe(subscriber: Flow.Subscriber[_ >: Int]): Unit =
    subscriber.onSubscribe(new SimpleSubscription(subscriber.asInstanceOf[RecordingSubscriber], values))

@main def javalibConcurrentFlow(): Unit =
  val subscriber = new RecordingSubscriber()
  new SimplePublisher(Array(1, 2, 3)).subscribe(subscriber)
  println("publisher:" + subscriber.snapshot())
