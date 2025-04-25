def main(): Unit =
  val xs: array[i32] = newArray[i32](10, 0)
  val t1 = xs(2)
  #i32println(t1)
  xs(2) = 42
  #i32println(xs(2))
  #i32println(xs.size)

  def go(current: i32): Unit =
    if current < xs.size then
      xs(current) = 100
      go(current + 1)

  go(0)

  def go(current: i32): Unit =
    if current < xs.size then
      #i32println(xs(current))
      go(current + 1)

  go(0)
