struct Var(var data: i32)
def main(): Unit =
  val s: String^ = "hello, world"
  putStrLn(s)
  putStrLn("This is hello from Cavia!!")
  putStrLn("hello, ".concat("world !!"))
  val sum = Var(0)
  val elapsed = benchmark(() => (0.until(100000)).iterate((i: i32) => sum.data = sum.data + i))
  putStrLn("Time elapsed (ms):")
  #i32println(elapsed)
  ()
