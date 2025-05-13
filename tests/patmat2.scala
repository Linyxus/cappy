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
def main(): Unit =
  val xs1 = Cons(0, Cons(1, Cons(2, Nil[i32]())))
  xs1.foreach: (n: i32) =>
    #i32println(n)
