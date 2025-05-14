def id[T](x: T): T = x
def app1[T](x: T)[cap C](op: T ->{C} Unit): Unit = op(x)
def app2[T](x: T)[U](op: T => U): U = op(x)  // error
def app3[T][U](x: T, f: T => U): U = f(x)   // error
def app4[T, U](x: T, f: T => U): U = f(x)   // ok
def main(): Unit = ()
