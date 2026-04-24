@main def collectionsList(): Unit =
  val xs = List(0, 1, 2)
  val ys = xs.reverse
  println("reverse:" + ys.head + ":" + ys.tail.head + ":" + ys.tail.tail.head + ":" + ys.tail.tail.tail.isEmpty)

  val nil: List[Int] = Nil
  println("nil:" + nil.isEmpty + ":" + (nil eq Nil))

  val singleton = 3 :: Nil
  println("singleton:" + singleton.tail.isEmpty + ":" + singleton.reverse.head)

  val zs = xs ++ ys
  println("concat-to-string:" + zs.toString)
  println("concat-plus:" + zs)
