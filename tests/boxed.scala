struct IO()
struct Box[T](x: T)
struct Box1(x: Box[IO^])
def foo(b: Box1^): Unit =
  val t1 = b
  val t2 = b.x
  ()
def bar(b: Box[Box[IO^]]): Unit =
  val t1 = b
  val t2 = b.x
  val t3 = b.x.x
def main(): Unit =
  ()
