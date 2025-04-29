extension [T](self: array[T]^)
  def mapInplace(op: T => T): Unit =
    def recur(x: i32): Unit =
      if x < self.size then
        self(x) = op(self(x))
        recur(x+1)
    recur(0)

  def foreach(op: T => Unit): Unit =
    def f(x: T): T =
      op(x)
      x
    self.mapInplace(f)

  def clone(): array[T]^ =
    val head = self(0)
    val res = newArray(self.size, head)
    def recur(idx: i32): Unit =
      if idx < self.size then
        res(idx) = self(idx)
    recur(0)
    res

def main(): Unit =
  val arr = newArray(32, 42)
  val arr1 = arr.clone()
  arr1.foreach((x: i32) => #i32println(x))
  ()

