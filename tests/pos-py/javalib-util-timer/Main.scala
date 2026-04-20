import java.util.{Timer, TimerTask}
import scala.python.runtime.PyTime

// Timer tests are tolerance-based, not wall-clock-exact, per
// `notes/javalib-timer-threading.md`. Every assertion asks "did the
// timer eventually fire?" / "did cancel() stop future firings?" —
// never "did it fire in exactly N milliseconds" or "exactly N
// times". Keeps the test stable on slow/loaded CI.

// Spin until `p()` is true or `totalBudget` seconds elapse. Short
// sleeps between checks let the Python timer thread preempt.
private def waitUntil(totalBudget: Double)(p: () => Boolean): Boolean =
  val start = PyTime.perf_counter_ns()
  val budgetNs = (totalBudget * 1e9).toLong
  var hit = p()
  while !hit && (PyTime.perf_counter_ns() - start) < budgetNs do
    PyTime.sleep(0.005)
    hit = p()
  hit

final class CountingTask extends TimerTask:
  var count = 0
  def run(): Unit =
    count += 1

@main def javalibUtilTimer(): Unit =
  // One-shot: assert it fired at least once within a generous budget.
  val onceTimer = new Timer()
  val onceTask = new CountingTask()
  onceTimer.schedule(onceTask, 20L)
  val fired = waitUntil(1.0)(() => onceTask.count >= 1)
  println("schedule:" + fired)

  // Repeating: wait until at least 2 firings, then cancel, then verify
  // the count doesn't grow after cancellation.
  val repeatingTimer = new Timer()
  val repeatingTask = new CountingTask()
  repeatingTimer.schedule(repeatingTask, 10L, 25L)
  val fired2 = waitUntil(1.0)(() => repeatingTask.count >= 2)
  repeatingTimer.cancel()
  val observed = repeatingTask.count
  // Even after the cancel, give any in-flight firing a chance to
  // complete before we snapshot; then assert no further firings.
  PyTime.sleep(0.1)
  println("cancel:" + fired2 + ":" + (repeatingTask.count == observed))
