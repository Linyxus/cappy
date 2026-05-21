package dotty.tools.benchmarks.py.dispatch

/** Chained wrapper objects: each logical `count` call re-dispatches through the
 *  full delegation chain. depth1/depth2/depth4 isolate the additive per-call
 *  virtual-dispatch cost as delegation depth grows. */
trait Counter:
  def count(x: Int): Int

class BaseCounter extends Counter:
  def count(x: Int): Int = x

class LoggingCounter(val inner: Counter, val bias: Int) extends Counter:
  def count(x: Int): Int = inner.count(x) + bias

class CachingCounter(val inner: Counter, val offset: Int) extends Counter:
  def count(x: Int): Int = inner.count(x) * offset

class ClampingCounter(val inner: Counter, val limit: Int) extends Counter:
  def count(x: Int): Int =
    val r = inner.count(x)
    if r > limit then limit else r

class DecoratorChainBench:
  var size: Int = 0
  var depth1: Array[Counter] = Array.empty
  var depth2: Array[Counter] = Array.empty
  var depth4: Array[Counter] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    depth1 = Array.tabulate(size)(_ => BaseCounter())
    depth2 = Array.tabulate(size)(i => LoggingCounter(BaseCounter(), i % 5 + 1))
    depth4 = Array.tabulate(size) { i =>
      ClampingCounter(
        CachingCounter(LoggingCounter(BaseCounter(), i % 5 + 1), i % 3 + 2),
        100000
      )
    }

  val operations: Map[String, () => Any] = Map(
    "depth1" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += depth1(i).count(i % 1024)
        i += 1
      s
    },
    "depth2" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += depth2(i).count(i % 1024)
        i += 1
      s
    },
    "depth4" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += depth4(i).count(i % 1024)
        i += 1
      s
    },
  )

@main def main(): Unit = ()
