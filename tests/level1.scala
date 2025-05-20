struct IO()
extension [R](op: IO^ => R)
  def useIO: R = op(IO())
def test1(): Unit =
  val id = (x: IO^) => x
  val t1 = id
def main(): Unit =
  putStrLn("hello world")

