enum ArList[T]:
  case Nil()
  case Cons(head: T, tail: Ar[ArList[T]]^)
def main(): Unit =
  putStrLn("Hello, world")
