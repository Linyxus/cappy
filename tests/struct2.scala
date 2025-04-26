struct Pair[A, B](var x: A, var y: B)
def main(): Unit =
  val p = Pair[i32, array[i32]^](0, newArray[i32](10, 42))
  #i32println(p.x)
  p.x = 42
  #i32println(p.x)
  def go(cur: i32): Unit =
    if cur < p.y.size then
      p.y(cur) = 100
      go(cur+1)
  go(0)
  def go(cur: i32): Unit =
    if cur < p.y.size then
      p.x = p.x + p.y(cur)
      #i32println(p.y(cur))
      go(cur+1)
  go(0)
  #i32println(p.x)
