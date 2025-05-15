enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
def enumerate(n: i32): List[i32]^ =
  if n < 0 then Nil[i32]()
  else Cons(n, enumerate(n-1))
extension [T](xs: List[T]^)
  def map[U](f: T => U): List[U]^ =
    xs match
      case Nil() => Nil[U]()
      case Cons(x, xs) =>
        Cons(f(x), xs.map[U](f))
  def foreach(op: T => Unit): Unit =
    xs.map[Unit](op)
    ()
  def foldRight[R](init: R)(op: (T, R) => R): R =
    xs match
      case Nil() => init
      case Cons(x, xs) =>
        op(x, xs.foldRight[R](init)(op))
  def zipWith[U, R](ys: List[U]^)(op: (T, U) => R): List[R]^ =
    xs match
      case Nil() => Nil[R]()
      case Cons(x, xs) => ys match
        case Nil() => Nil[R]()
        case Cons(y, ys) => Cons(op(x, y), xs.zipWith[U, R](ys)(op))
def main(): Unit =
  putStrLn("hello, world")
  val xs = enumerate(100)
  val sum = xs.foldRight[i32](0): (n: i32, acc: i32) =>
    n + acc
  #i32println(sum)
  val ys = enumerate(100).map[i32]: (n: i32) =>
    n + 1
  val zs = xs.zipWith[i32, i32](ys): (x: i32, y: i32) =>
    x * y
  val sum1 = zs.foldRight[i32](0): (n: i32, acc: i32) =>
    n + acc
  #i32println(sum1)

