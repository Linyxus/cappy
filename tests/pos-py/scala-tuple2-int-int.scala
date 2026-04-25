@main def run(): Unit =
  // Literal Tuple2[Int, Int] — pre-fix this triggered SpecializeTuples
  // and produced an unresolved reference to scala.Tuple2$mcII$sp.
  val p: Tuple2[Int, Int] = (1, 2)
  println(p._1)
  println(p._2)
  println(p)

  // List[(Int, Int)] round-trip exercises the same shape inside a
  // collection literal.
  val pairs: List[(Int, Int)] = List((1, 2), (3, 4))
  println(pairs.head._1)
  println(pairs.head._2)
  println(pairs.last._1)
  println(pairs.last._2)

  // Tuple2[Long, Long] — different specialization variant
  // (Tuple2$mcJJ$sp), exercises the same gate.
  val q: Tuple2[Long, Long] = (10L, 20L)
  println(q._1)
  println(q._2)

  // Mixed primitive specialization (Int, Double) — yet another variant.
  val r: Tuple2[Int, Double] = (3, 0.5)
  println(r._1)
  println(r._2)
