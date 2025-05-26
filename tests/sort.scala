enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
extension [T](xs: List[T]^)
  def map[U](f: T => U): List[U]^ =
    xs match
      case Nil() => Nil[U]()
      case Cons(x, xs) =>
        Cons(f(x), xs.map[U](f))
  def foreach(f: T => Unit): Unit =
    xs.map[Unit]: (x: T) =>
      f(x)
  def filter(f: T => bool): List[T]^ =
    xs match
      case Nil() => Nil[T]()
      case Cons(x, xs) =>
        if f(x) then Cons(x, xs.filter(f))
        else xs.filter(f)
  def reverse: List[T]^ =
    def recur(xs: List[T]^, consume acc: List[T]^): List[T]^ =
      xs match
        case Nil() => acc
        case Cons(x, xs) =>
          recur(xs, Cons(x, acc))
    recur(xs, Nil[T]())
  def concat(other: List[T]^): List[T]^{other, cap} =
    def recur(todos: List[T]^, acc: List[T]^): List[T]^{acc, cap} =
      todos match
        case Nil() => acc
        case Cons(todo, todos) =>
          recur(todos, Cons(todo, acc))
    val t1 = xs.reverse
    recur(xs.reverse, other)
  def length: i32 =
    xs match
      case Nil() => 0
      case Cons(_, xs) => 1 + xs.length
  def at(idx: i32): T =
    xs match
      case Cons(x, xs) =>
        if idx <= 0 then x
        else xs.at(idx-1)
def show(xs: List[i32]^): Unit =
  putStrLn("[")
  xs.foreach: (n: i32) =>
    putStr(", ")
    #i32println(n)
  putStrLn("]")
def quicksort(xs: List[i32]^): List[i32]^ =
  xs match
    case Nil() => Nil[i32]()
    case Cons(x, Nil()) => Cons(x, Nil[i32]())
    case _ =>
      val mid = xs.at(xs.length / 2)
      val left = xs.filter((i: i32) => i < mid)
      val right = xs.filter((i: i32) => i > mid)
      val same = xs.filter((i: i32) => i == mid)
      quicksort(left).concat(same.concat(quicksort(right)))
def enumerate(start: i32, end: i32): List[i32]^ =
  def recur(cur: i32, consume acc: List[i32]^): List[i32]^ =
    if cur < start then acc
    else recur(cur-1, Cons(cur, acc))
  recur(end-1, Nil[i32]())
def main(): Unit =
  putStrLn("Hello, world")
  val xs1 = Cons(1, Cons(2, Cons(3, Nil[i32]())))
  val xs2 = enumerate(0, 3000)
  //show(xs2)
  val xs3 = xs2.reverse
  //show(xs3)
  val res = benchmark: () =>
    val xs4 = quicksort(xs3)
    //show(xs4)
  #i32println(res)
