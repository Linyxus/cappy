struct P[A, B](a: A, b: B)
struct IO()
def use(x: IO^): Unit = ()
def foo(p: P[IO^, IO^]^): Unit = 
  val t0 = p
  val t1 = () => use(p.a)
  ()
def main(): Unit =
  val a = IO()
  val b = IO()
  val p1 = P[box IO^{a}, box IO^{b}](a, b)
