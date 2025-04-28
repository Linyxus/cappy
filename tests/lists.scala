struct Ref(x: i32)
struct List[T](head: T^, tail: List[T]^)
//def useResource[T](op: T^ => Unit): Unit = ()
val nil: List[Ref] = sorry()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  val c = Ref(0)
  val t1 = List[Ref](c, nil)
  val t2 = List[Ref](b, t1)
  val t3 = List[Ref](a, t2)
  val t4: Ref^{b} = t3.tail.head
  val t5: Ref^{c} = t3.tail.tail.head
  ()
