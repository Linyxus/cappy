struct Ref()
def main(): Unit =
  val x = Ref()
  def foo(x: Ref^): Ref^ = x  // error

