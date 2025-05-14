enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
enum Maybe[+T]:
  case Yes(value: T)
  case No()
struct Res[S, R](nextState: S^, res: R)
struct Iterator[S, R]:
  var state: S
  val compute: S => Maybe[Res[S, R]^]
def main(): Unit =
  putStrLn("hello")
