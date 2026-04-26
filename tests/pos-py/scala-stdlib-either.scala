// Broad coverage of scala.util.Either operations.
// Left/Right toString is stable; print only stable derived values.

@main def scalaStdlibEither(): Unit =
  val r: Either[String, Int] = Right(42)
  val l: Either[String, Int] = Left("boom")

  // 1. construction
  println("build:" + r + ":" + l)

  // 2. predicates
  println("access:" + r.isRight + ":" + r.isLeft + ":" + l.isRight + ":" + l.isLeft)

  // 3. map / flatMap (right-biased)
  val mappedR = r.map(_ + 1)
  val mappedL = l.map(_ + 1)
  val flatR = r.flatMap(x => Right(x * 2))
  val flatRtoL: Either[String, Int] = r.flatMap(_ => Left("nope"))
  val flatL = l.flatMap(x => Right(x * 2))
  println("transform:" + mappedR + ":" + mappedL + ":" + flatR + ":" + flatRtoL + ":" + flatL)

  // 4. swap (exercises type-parameter shuffling)
  val swappedR = r.swap
  val swappedL = l.swap
  println("swap:" + swappedR + ":" + swappedL)

  // 5. exists / forall
  println("exists:" + r.exists(_ == 42) + ":" + l.exists(_ == 42))
  println("forall:" + r.forall(_ > 0) + ":" + l.forall(_ > 0))

  // 6. aggregate: getOrElse / fold
  println("getOrElse:" + r.getOrElse(-1) + ":" + l.getOrElse(-1))
  println("fold:" + r.fold(s => "L:" + s, i => "R:" + i) + ":" + l.fold(s => "L:" + s, i => "R:" + i))

  // 7. convert
  println("toOption:" + r.toOption + ":" + l.toOption)
  println("toSeq:" + r.toSeq + ":" + l.toSeq)
