package dotty.tools.benchmarks.py.datastruct

import scala.annotation.tailrec

/** Singly-linked cons list built from user case classes. Allocation-heavy
 *  build/reverse plus pointer-chasing tail-recursive traversals. The
 *  traversals use accumulator-passing `@tailrec` helpers so dotty's
 *  tail-call phase lowers them to loops — deep lists (size >= 1024) would
 *  otherwise exceed CPython's recursion limit on the Python backend. */
sealed trait ConsList[+A]
case object ConsNil extends ConsList[Nothing]
case class Cons[+A](head: A, tail: ConsList[A]) extends ConsList[A]

class ConsListBench:
  var size: Int = 0
  var list: ConsList[Int] = ConsNil

  def build(n: Int): ConsList[Int] =
    var acc: ConsList[Int] = ConsNil
    var i = 0
    while i < n do
      acc = Cons(i, acc)
      i += 1
    acc

  def sumFold(xs: ConsList[Int]): Int =
    @tailrec def loop(ys: ConsList[Int], acc: Int): Int = ys match
      case ConsNil          => acc
      case Cons(head, tail) => loop(tail, acc + head)
    loop(xs, 0)

  def length(xs: ConsList[Int]): Int =
    @tailrec def loop(ys: ConsList[Int], acc: Int): Int = ys match
      case ConsNil       => acc
      case Cons(_, tail) => loop(tail, acc + 1)
    loop(xs, 0)

  def reverse(xs: ConsList[Int]): ConsList[Int] =
    var acc: ConsList[Int] = ConsNil
    var cur = xs
    var go = true
    while go do
      cur match
        case ConsNil          => go = false
        case Cons(head, tail) => acc = Cons(head, acc); cur = tail
    acc

  def setup(size: Int): Unit =
    this.size = size
    list = build(size)

  val operations: Map[String, () => Any] = Map(
    "build"   -> (() => build(size)),
    "sumFold" -> (() => sumFold(list)),
    "length"  -> (() => length(list)),
    "reverse" -> (() => reverse(list)),
  )

@main def main(): Unit = ()
