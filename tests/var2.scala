struct Cap()
struct Box1[+A](x: A)
def main(): Unit =
  val a = Cap()
  val b = Cap()
  val t1: Box1[Cap^{a}] = sorry()
  val t2: Box1[Cap^{a,b}] = t1
  ()
