struct Counter(var x: i32)
def modify(c: Counter^, f: i32 => i32): i32 =
  val oldValue = c.x
  c.x = f(oldValue)
  oldValue
def inc(c: Counter^): i32 = modify(c, (x: i32) => #i32add(x, 1))
def show(c: Counter^): Unit = #i32println(c.x)
def main(): Unit =
  val c = Counter(0)
  show(c)
  inc(c)
  show(c)
  inc(c)
  show(c)
