struct A()
struct P(x: A^, y: A^)
def sep(x: A^, y: A^): Unit = ()
def kill(consume x: A^): Unit = ()
def main(): Unit =
  def foo(consume p: P^) =
    p match
      case P(x, y) => x

