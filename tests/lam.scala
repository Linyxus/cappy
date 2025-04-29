struct Ref(var data: i32)
def main(): Unit =
  val a = Ref(0)
  val t1 = () => () => a
  def t2 = 1
  def t3 = a
  def t4[T](x: T): T =
    a
    x
  ()
