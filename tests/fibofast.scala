def main(): Unit =
  val limit = 100
  val cache = newArray[i32](limit, 0)
  def go(x: i32): Unit =
    if x <= 2 then
      cache(x - 1) = 1
      #i32println(cache(x - 1))
      go(x + 1)
    else if x <= limit then
      cache(x - 1) = cache(x - 2) + cache(x - 3)
      #i32println(cache(x - 1))
      go(x + 1)
  go(1)

