package dotty.tools.benchmarks.py.datastruct

/** Circular buffer over Array[Int] with mutable head/tail/count fields on a
 *  plain (non-case) class. Stresses Array read/write, modulo arithmetic, and
 *  var-field getter/setter overhead; no pattern-match dispatch. */
class RingBuf(cap: Int):
  val data: Array[Int] = new Array[Int](cap)
  var head: Int = 0
  var tail: Int = 0
  var count: Int = 0
  def capacity: Int = cap

  def enqueue(v: Int): Unit =
    data(tail) = v
    tail = (tail + 1) % cap
    count += 1

  def dequeue(): Int =
    val v = data(head)
    head = (head + 1) % cap
    count -= 1
    v

  def peek: Int = data(head)

class RingBufferBench:
  var size: Int = 0
  var buf: RingBuf = new RingBuf(1)

  def setup(size: Int): Unit =
    this.size = size
    buf = new RingBuf(size + 1)
    var i = 0
    while i < size / 2 do
      buf.enqueue(i * 7 + 1)
      i += 1

  val operations: Map[String, () => Any] = Map(
    "enqDeq" -> { () =>
      val b = new RingBuf(size + 1)
      var i = 0
      while i < size do
        b.enqueue(i * 7 + 1)
        i += 1
      var last = 0
      while b.count > 0 do
        last = b.dequeue()
      last
    },
    "slideWindow" -> { () =>
      val b = new RingBuf(8)
      b.enqueue(1)
      var s = 0L
      var i = 0
      while i < size do
        b.enqueue((i * 13 + 5) & 0x7fffffff)
        s += b.dequeue().toLong
        i += 1
      s
    },
    "peekLoop" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += buf.peek.toLong
        i += 1
      s
    },
  )

@main def main(): Unit = ()
