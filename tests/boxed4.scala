struct Fun[-A, +B](run: A => B)
struct IO()
def f1(f: Fun[IO^, IO^]): Unit =
  val t1 = f
  val t2 = f.run
  ()
def main(): Unit =
  ()
