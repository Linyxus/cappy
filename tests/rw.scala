def main(): Unit =
  val x = #i32read()
  val y = #i32add(x, x)
  #i32println(y)
