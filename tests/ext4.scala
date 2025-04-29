extension [T](x: T)
  def use(op: (x: T) => Unit): Unit = op(x)
  def foo(): Unit = ()
  def bar(i: i32, j: i32, k: i32): Unit = #i32println(i * j + k)
def main(): Unit =
  val t = 0
  t.use((i: i32) => #i32println(i))
  t.foo()
  t.bar(100, 2, 42)
  ()
