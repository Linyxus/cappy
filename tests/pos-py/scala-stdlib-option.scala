// Broad coverage of scala.Option monadic operations.
// Print only stable derived values; Some/None toString is stable.

@main def scalaStdlibOption(): Unit =
  // 1. construction variants
  val s: Option[Int] = Some(42)
  val n: Option[Int] = None
  val empty = Option.empty[Int]
  val fromVal = Option(7)
  val fromNull = Option[String](null)
  println("build:" + s + ":" + n + ":" + empty + ":" + fromVal + ":" + fromNull)

  // 2. accessors / predicates
  println("access:" + s.get + ":" + s.isDefined + ":" + s.isEmpty + ":" + n.isDefined + ":" + n.isEmpty)

  // 3. map / flatMap
  val mapped = s.map(_ + 1)
  val flatSome = s.flatMap(x => Some(x * 2))
  val flatNone = s.flatMap(_ => None)
  val mappedNone = n.map(_ + 1)
  println("transform:" + mapped + ":" + flatSome + ":" + flatNone + ":" + mappedNone)

  // 4. filter / exists / forall
  val keep = s.filter(_ > 0)
  val drop = s.filter(_ < 0)
  println("filter:" + keep + ":" + drop)
  println("exists:" + s.exists(_ == 42) + ":" + n.exists(_ == 42))
  println("forall:" + s.forall(_ > 0) + ":" + n.forall(_ > 0))

  // 5. aggregate: getOrElse / fold
  println("getOrElse:" + s.getOrElse(-1) + ":" + n.getOrElse(-1))
  println("fold:" + s.fold(-1)(_ + 1) + ":" + n.fold(-1)(_ + 1))

  // 6. convert
  println("toList:" + s.toList + ":" + n.toList)
