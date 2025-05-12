enum Option[+T]:
  case Ok(x: T)
  case None()
enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
enum Either[+A, +B]:
  case Left(a: A)
  case Right(b: B)
def main(): Unit = ()
