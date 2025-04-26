struct Pair[A, B](var x: A, var y: B)

def iterate(size: i32, op: i32 => Unit): Unit =
  def go(cur: i32): Unit =
    if cur < size then
      op(cur)
      go(cur+1)
  go(0)

def showArray(xs: array[i32]^): Unit =
  iterate(xs.size, (idx: i32) => #i32println(xs(idx)))

def mapArray(xs: array[i32]^, f: i32 => i32): Unit =
  def go(cur: i32): Unit =
    if cur < xs.size then
      xs(cur) = f(xs(cur))
      go(cur+1)
  go(0)

def main(): Unit =
  val p = Pair[i32, array[i32]^](0, newArray[i32](10, 42))
  p.x = 42
  iterate(p.y.size, (idx: i32) => p.y(idx) = 100)
  showArray(p.y)
  mapArray(p.y, (x: i32) => x + 1)
  showArray(p.y)
  mapArray(p.y, (x: i32) => x * 2)
  showArray(p.y)
  
  p.x = 0
  def f(x: i32): i32 =
    p.x = p.x + x
    x
  mapArray(p.y, f)
  #i32println(p.x)
