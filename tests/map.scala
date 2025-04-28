def mapInplace(xs: array[i32]^, f: i32 => i32): Unit =
  def go(cur: i32): Unit =
    if cur < xs.size then
      xs(cur) = f(xs(cur))
      go(cur + 1)
  go(0)

def foreach(xs: array[i32]^, op: i32 => Unit): Unit =
  val f = (x: i32) =>
    op(x)
    x
  mapInplace(xs, f)

def main(): Unit =
  def print(x: i32): Unit =
    #i32println(x)

  val xs = newArray[i32](10, 0)
  foreach(xs, print)

  mapInplace(xs, (x: i32) => x + 1)
  foreach(xs, print)

  def foo(x: i32): Unit =
    xs(0) = x
  //foreach(xs, (x: i32) => xs(0) = x) // error

