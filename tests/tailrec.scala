def bench(runner: () => Unit): i32 =
  val start = #perfcounter()
  runner()
  val end = #perfcounter()
  end - start
def go1(i: i32, acc: i32): i32 =
  if i <= 0 then acc
  else go1(i - 1, acc + i)
def main(): Unit =
  def go(i: i32, acc: i32): i32 =
    if i <= 0 then acc
    else go(i - 1, acc + i)

  val elapsed = bench(() => #i32println(go(30000000, 0)))
  #i32println(elapsed)

  val elapsed = bench(() => #i32println(go1(30000000, 0)))
  #i32println(elapsed)
