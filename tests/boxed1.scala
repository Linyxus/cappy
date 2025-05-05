struct IO()
//def f1(x: box IO^): Unit = ()
//def f2(consume x: box IO^): IO^ = x
def box(x: IO^): box IO^{x} = x
def main(): Unit = ()

