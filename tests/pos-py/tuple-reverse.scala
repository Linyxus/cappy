@main def run(): Unit =
  println(EmptyTuple.reverse)
  println(Tuple1(42).reverse)
  println((1, 2, 3).reverse)
  println((1, 2, 3, 4, 5, 6).reverse)
  println((1, 2, 3).reverse.reverse == (1, 2, 3))
  println((1, 2, 3, 4, 5).reverse.reverse == (1, 2, 3, 4, 5))
