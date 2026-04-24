@main def scalaCollectionsOps(): Unit =
  val xs = List(1, 2, 3, 4, 5)
  println("fold:" + xs.foldLeft(0)((acc, n) => acc + n) + ":" + xs.foldLeft("z")((acc, n) => acc + n.toString))
  println("exists-forall:" + xs.exists(n => n == 3) + ":" + xs.exists(n => n > 9) + ":" + xs.forall(n => n > 0) + ":" + xs.forall(n => n < 5))

  val zipped = List("a", "b", "c").zip(List(1, 2))
  println("zip:" + zipped.head._1 + ":" + zipped.head._2 + ":" + zipped.tail.head._1 + ":" + zipped.tail.head._2 + ":" + zipped.size)
  println("mkString:" + xs.mkString("[", "|", "]") + ":" + List.empty[Int].mkString("[", "|", "]"))

  val iterator = xs.iterator
  val firstHasNext = iterator.hasNext
  val first = iterator.next()
  val second = iterator.next()
  val afterTwoHasNext = iterator.hasNext
  println("iterator:" + firstHasNext + ":" + first + ":" + second + ":" + afterTwoHasNext)
