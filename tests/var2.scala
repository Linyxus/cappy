struct Cap()
struct Box1[+A](x: A)
struct Id[A](f: A => A)
def forall(b1: Box1[Cap^], b2: Box1[Cap^]): Unit = ()
def main(): Unit =
  val a = Cap()
  val b = Cap()
  val t1: Box1[Cap^{a}] = sorry()
  val t2: Box1[Cap^{b}] = sorry()
  val t3: Box1[Cap^{a,b}] = t1
  //val t2: Box1[Cap^{a,b}] = t1
  //val t2: (z: Cap^{a}) -> Cap^{b} = sorry()
  forall(t1, t3)
  ()
