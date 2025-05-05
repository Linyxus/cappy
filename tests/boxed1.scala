struct IO()
//def f1(x: box IO^): Unit = ()
//def f2(consume x: box IO^): IO^ = x
//def box(x: IO^): () -> box IO^{x} = () => #box(x)
def use(x: IO^): Unit = ()
def foo(x: box IO^): Unit =
  val t1 = x
  val t2 = () => #unbox(x)
  val t3 = () => use(x)
def bar(x: IO^): box IO^{x} = x
def main(): Unit = ()

