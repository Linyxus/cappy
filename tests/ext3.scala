extension [T](x: array[T]^)
  def show: Unit = ()
def main(): Unit =
  val BUF_SIZE: i32 = 256
  val xs = newArray(BUF_SIZE, false)
  val ys = newArray(BUF_SIZE, 0)
  val zs = newArray(BUF_SIZE, ys)
  xs.show
  ys.show
  zs.show
  ()
