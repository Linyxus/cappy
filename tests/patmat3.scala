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
def main(): Unit =
  putStrLn("hello, world")
