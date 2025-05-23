struct IO()
def foo(x: IO^, y: IO^{x}): Unit = 0
def main(io: IO^): Unit =
  val i: i32 = 0
  foo(io, i)  // error
