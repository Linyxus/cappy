extension [T](xs: array[T]^)
  def show: Unit = #i32println(xs.size)
  def foreach(op1: T => Unit): Unit =
    def f(x: T): T =
      op1(x)
      x
    xs.mapInplace(f)
  def mapInplace(op2: T => T): Unit =
    def recur(now: i32): Unit =
      if now < xs.size then
        xs(now) = op2(xs(now))
        recur(now + 1)
    recur(0)
def main(): Unit =
  val BUF_SIZE: i32 = 8
  val xs = newArray(BUF_SIZE, 42)
  xs.show
  xs.foreach((x: i32) => #i32println(x))
