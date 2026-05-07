@main def run(): Unit =
  val e1: EmptyTuple = EmptyTuple
  val e2: EmptyTuple = Tuple()
  println(e1.size)
  println(e2.size)
  println(e1 == e2)
  println(e1)
  println(e1.hashCode == e2.hashCode)
