struct Box[X](data: X)
struct Foo()
def main(): Unit =
  val f = Foo()
  val x = Box[Foo^{f}](f)

