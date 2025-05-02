def go1(i: i32, acc: i32): i32 =
  if i <= 0 then acc
  else go1(i - 1, acc + i)
def main(): Unit =
  def go(i: i32, acc: i32): i32 =
    if i <= 0 then acc
    else go(i - 1, acc + i)
  val start = #perfcounter()
  #i32println(go(10000000, 0))
  #i32println(#perfcounter() - start)
