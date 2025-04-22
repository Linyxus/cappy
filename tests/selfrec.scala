def main[IO](a: IO^{cap}): i32 =
  def f(): Unit = 
    a
    f()
  0
