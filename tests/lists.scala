struct Ref(x: i32)
struct List(head: Ref^, tail: List^)
val nil: List = sorry()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  val c = Ref(0)
  val t1 = List(c, nil)
  val t2 = List(b, t1)
  val t3 = List(a, t2)
  val t4: Ref^{b} = t3.tail.head
  val t5: Ref^{c} = t3.tail.tail.head
  ()
