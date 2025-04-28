extension (xs: array[i32]^)
  def show(): Unit =
    xs.foreach((x: i32) => #i32println(x))
  def foreach(op: i32 => Unit): Unit =
    def go(cur: i32): Unit =
      if cur < xs.size then
        op(xs(cur))
        go(cur + 1)
    go(0)

def main(): Unit =
  val xs = newArray[i32](10, 0)
  xs.show()
  ()
