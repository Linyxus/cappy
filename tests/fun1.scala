type Fun[-A, +B] = (z: A) -> B
struct IO()
def foo(f: Fun[i32, IO^]^): Unit =
  val t1 = f
  val t2 = f(0)
  //val t3 = #unbox(t2)
def main(): Unit =
  ()
