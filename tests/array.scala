def main(): Unit =
  val xs: array[i32] = newArray[i32](10, 0)
  val t1 = xs(4)
  xs(0) = 42
  xs(100) = 100
