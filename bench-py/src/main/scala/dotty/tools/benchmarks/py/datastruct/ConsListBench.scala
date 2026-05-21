package dotty.tools.benchmarks.py.datastruct

/** Singly-linked cons list built from user case classes. Allocation-heavy
 *  build/reverse plus pointer-chasing recursive traversals. */
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

  def sumFold(xs: ConsList[Int]): Int = xs match
    case ConsNil          => 0
    case Cons(head, tail) => head + sumFold(tail)

  def length(xs: ConsList[Int]): Int = xs match
    case ConsNil       => 0
    case Cons(_, tail) => 1 + length(tail)

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
