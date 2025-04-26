struct Ref(var data: i32)
//struct RefPair(x: Ref^, y: Ref^)
//def par(op1: () => Unit, op2: () => Unit): Unit = ()
def foo(f: () => Ref^): Unit = ()
def main(): Unit = ()

