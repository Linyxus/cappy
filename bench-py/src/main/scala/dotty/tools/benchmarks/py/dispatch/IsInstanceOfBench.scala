package dotty.tools.benchmarks.py.dispatch

/** Sealed-trait type-test dispatch in pattern-match form vs explicit
 *  isInstanceOf/asInstanceOf form. Each has a megamorphic (4-way) and a
 *  monomorphic variant to isolate the type-test machinery overhead. */
sealed trait Event
class ClickEvent(val x: Int, val y: Int) extends Event
class KeyEvent(val code: Int) extends Event
class ScrollEvent(val delta: Int) extends Event
class ResizeEvent(val w: Int, val h: Int) extends Event

class IsInstanceOfBench:
  var size: Int = 0
  var events: Array[Event] = Array.empty
  var clicks: Array[Event] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    events = Array.tabulate(size) { i =>
      (i % 4) match
        case 0 => ClickEvent(i, i % 7)
        case 1 => KeyEvent(i % 128)
        case 2 => ScrollEvent(i % 11 - 5)
        case _ => ResizeEvent(i % 9 + 1, i % 6 + 1)
    }
    clicks = Array.tabulate(size)(i => ClickEvent(i, i % 7))

  private def viaMatch(e: Event): Int = e match
    case c: ClickEvent  => c.x + c.y
    case k: KeyEvent    => k.code
    case s: ScrollEvent => s.delta
    case r: ResizeEvent => r.w * r.h

  private def viaIsInstance(e: Event): Int =
    if e.isInstanceOf[ClickEvent] then
      val c = e.asInstanceOf[ClickEvent]; c.x + c.y
    else if e.isInstanceOf[KeyEvent] then
      e.asInstanceOf[KeyEvent].code
    else if e.isInstanceOf[ScrollEvent] then
      e.asInstanceOf[ScrollEvent].delta
    else
      val r = e.asInstanceOf[ResizeEvent]; r.w * r.h

  val operations: Map[String, () => Any] = Map(
    "patMatchMega" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += viaMatch(events(i))
        i += 1
      s
    },
    "patMatchMono" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += viaMatch(clicks(i))
        i += 1
      s
    },
    "isInstanceMega" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += viaIsInstance(events(i))
        i += 1
      s
    },
    "isInstanceMono" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += viaIsInstance(clicks(i))
        i += 1
      s
    },
  )

@main def main(): Unit = ()
