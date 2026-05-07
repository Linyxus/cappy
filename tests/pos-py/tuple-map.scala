@main def run(): Unit =
  val t = (1, 2, 3)
  val mapped = t.map[[X] =>> Option[X]]([X] => (x: X) => Some(x))
  println(mapped)
  println(mapped._1)
  println(mapped._2)
  println(mapped._3)

  val empty = EmptyTuple.map[[X] =>> Option[X]]([X] => (x: X) => Some(x))
  println(empty)
  println(empty == EmptyTuple)
