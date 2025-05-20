def main(): Unit =
  val xs1 = Cons(1, Cons(2, Nil[i32]()))
  xs1.foreach(#i32println)
