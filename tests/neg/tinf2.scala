struct A()
struct B()
def f1[T](x: T, y: T): Unit = ()
def test1(): Unit = f1(0, 0)  // ok
def test2(): Unit = f1(0, false)  // error
def test3(): Unit = f1(false, false)  // ok
def test4(): Unit = f1("hello", 0)  // error
def test5(): Unit = f1(A(), A())  // error
def test5(): Unit = f1(A(), B())  // error

