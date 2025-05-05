struct Box[T](unbox: T)
def run(op: Box[() => Unit]): Unit = 
  val t = () => op.unbox()
  ()
def foo(xs: Box[array[i32]^]): i32 =
  val t = () => xs.unbox(0)
  0
def main(): Unit =
  ()
