enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
extension [T](xs: List[T]^)
  def foreach(op: T => Unit): Unit =
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        op(h)
        t.foreach(op)
  def concat(other: List[T]^): List[T]^{cap, other} =
    xs match
      case Nil() => other
      case Cons(h, t) => Cons(h, t.concat(other))
  def snoc(elem: T): List[T]^ =
    xs match
      case Nil() => Cons(elem, Nil[T]())
      case Cons(t, h) => Cons(t, h.snoc(elem))
  def filter(pred: T => bool): List[T]^ =
    xs match
      case Nil() => Nil[T]()
      case Cons(h, t) =>
        if pred(h) then Cons(h, t.filter(pred))
        else t.filter(pred)
  def reverse: List[T]^ =
    xs match
      case Nil() => Nil[T]()
      case Cons(h, t) =>
        t.reverse.snoc(h)
  def reduceRight(init: T)(op: (T, T) => T): T =
    xs match
      case Nil() => init
      case Cons(x, xs) =>
        op(x, xs.reduceRight(init)(op))
def enumerate(n: i32): List[i32]^ =
  def go(n: i32, k: (consume xs: List[i32]^) => List[i32]^): List[i32]^ =
    if n < 0 then k(Nil[i32]())
    else
      def k1(consume acc: List[i32]^): List[i32]^ =
        Cons(n, k(acc))
      go(n-1, k1)
  def id(consume x: List[i32]^): List[i32]^ = x
  go(n, id)
def isEven(n: i32): bool = n % 2 == 0
def show(xs: List[i32]^): Unit =
  putStrLn(">>>>>")
  xs.foreach: (i: i32) =>
    #i32println(i)
  putStrLn("<<<<<")
def main(): Unit =
  putStrLn("hello, world")
  val xs1 = enumerate(10)
  show(xs1)
  val xs2 = xs1.filter(isEven)
  show(xs2)
  val xs3 = xs2.reverse
  show(xs3)
  val xs4 = xs2.concat(xs3)
  show(xs4)
  val sum = xs4.reduceRight(0): (a: i32, b: i32) =>
    a + b
  #i32println(sum)
  def compute(): Unit =
    val xs = enumerate(5000)
    val sum = xs.reduceRight(0)((a: i32, b: i32) => a + b)
    #i32println(sum)
  val elapsed = benchmark(compute)
  putStrLn("time elapsed:")
  #i32println(elapsed)


