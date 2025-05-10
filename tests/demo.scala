struct Ref(var data: i32)
struct Pair(fst: Ref^, snd: Ref^)
extension (self: Ref^)
  def update(op: i32 => i32): Unit =
    self.data = op(self.data)
def startThread(consume runner: () => Unit): Unit =
  ()
def par(op1: () => Unit, op2: () => Unit): Unit =
  ()
def main(): Unit =
  def inc(i: i32): i32 = i + 1
  val a = Ref(0)
  val b = Ref(0)
  val incA = () => a.update(inc)
  val incB = () => b.update(inc)
  def foo(x: i32): i32 =
    a.data = x
    x + 1
  //startThread(incA)
  par(incA, incB)
  ()
