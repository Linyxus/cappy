struct Ref()
def main(): Unit =
  val x = Ref()
  def foo: Ref^ = x  // error

