import scala.collection.mutable.HashMap

@main def scalaMutableHashmapRoundtrip(): Unit =
  val m = new HashMap[Int, Int]
  m(1) = 10
  m(2) = 20
  println(m.get(1))
  println(m.contains(1))
  println(m.size)
