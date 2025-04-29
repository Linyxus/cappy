struct Ref()
def main(): Unit =
  val x = Ref()
  //def foo: Ref^ = x
  def foo(x: Ref^): Ref^ = x

