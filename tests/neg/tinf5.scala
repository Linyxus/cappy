struct IO()
def main(): Unit =
  val f = (x: box IO^) => x
  def foo[X](g: X => X): Unit = ()
  foo((x: i32) => x)  // ok
  foo(f)  // error
