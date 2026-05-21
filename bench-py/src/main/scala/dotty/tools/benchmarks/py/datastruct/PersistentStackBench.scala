package dotty.tools.benchmarks.py.datastruct

/** Immutable persistent stack via covariant case-class cons cells. Tests
 *  +A/Nothing ADTs, per-push allocation, structural sharing, and
 *  pattern-match dispatch per pop/walk step. */
sealed trait PStack[+A]
case object PEmpty extends PStack[Nothing]
case class PFrame[+A](top: A, rest: PStack[A]) extends PStack[A]

class PersistentStackBench:
  var size: Int = 0
  var stack: PStack[Int] = PEmpty

  def push[A](s: PStack[A], v: A): PStack[A] = PFrame(v, s)

  def pop[A](s: PStack[A]): (A, PStack[A]) = s match
    case PFrame(top, rest) => (top, rest)
    case PEmpty            => throw new RuntimeException("empty")

  def sumTop(s: PStack[Int]): Long = s match
    case PEmpty            => 0L
    case PFrame(top, rest) => top.toLong + sumTop(rest)

  def length(s: PStack[Int]): Int = s match
    case PEmpty         => 0
    case PFrame(_, rest) => 1 + length(rest)

  def build(n: Int): PStack[Int] =
    var s: PStack[Int] = PEmpty
    var i = 0
    while i < n do
      s = push(s, i)
      i += 1
    s

  def setup(size: Int): Unit =
    this.size = size
    stack = build(size)

  val operations: Map[String, () => Any] = Map(
    "pushPop" -> { () =>
      var s: PStack[Int] = build(size)
      var last = 0
      var go = true
      while go do
        s match
          case PEmpty            => go = false
          case PFrame(top, rest) => last = top; s = rest
      last
    },
    "sumTop"    -> (() => sumTop(stack)),
    "copyStack" -> { () =>
      var copy: PStack[Int] = PEmpty
      var cur: PStack[Int] = stack
      var go = true
      while go do
        cur match
          case PEmpty            => go = false
          case PFrame(top, rest) => copy = push(copy, top); cur = rest
      length(copy)
    },
  )

@main def main(): Unit = ()
