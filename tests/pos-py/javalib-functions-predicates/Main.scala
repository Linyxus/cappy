import java.util.function.*

@main def javalibFunctionsPredicates(): Unit =
  val nonEmpty: Predicate[String] = value => value.length > 0
  val startsWithA: Predicate[String] = value => value.startsWith("a")
  val endsWithBang: Predicate[String] = value => value.endsWith("!")

  val ordered: BiPredicate[Int, Int] = (left, right) => left < right
  val distanceEven: BiPredicate[Int, Int] = (left, right) => (right - left) % 2 == 0
  val same: BiPredicate[Int, Int] = (left, right) => left == right

  val intPositive: IntPredicate = value => value > 0
  val intEven: IntPredicate = value => value % 2 == 0

  val longPositive: LongPredicate = value => value > 0L
  val longMultipleOfThree: LongPredicate = value => value % 3L == 0L

  val doublePositive: DoublePredicate = value => value > 0.0
  val doubleWhole: DoublePredicate = value => value == value.toLong.toDouble

  println("predicate-and:" + nonEmpty.and(startsWithA).test("atom"))
  println("predicate-or:" + startsWithA.or(endsWithBang).test("zoom!"))
  println("predicate-negate:" + startsWithA.negate().test("beta"))
  println(
    "predicate-isEqual:" +
      Predicate.isEqual[String]("scala").test("scala") + ":" +
      Predicate.isEqual[String](null).test(null)
  )
  println("bipredicate-and:" + ordered.and(distanceEven).test(1, 5))
  println("bipredicate-or:" + ordered.or(same).test(3, 3))
  println("bipredicate-negate:" + ordered.negate().test(4, 1))
  println(
    "int-predicate:" +
      intPositive.and(intEven).test(4) + ":" +
      intPositive.or(intEven).test(-2) + ":" +
      intEven.negate().test(3)
  )
  println(
    "long-predicate:" +
      longPositive.and(longMultipleOfThree).test(9L) + ":" +
      longPositive.or(longMultipleOfThree).test(0L) + ":" +
      longMultipleOfThree.negate().test(10L)
  )
  println(
    "double-predicate:" +
      doublePositive.and(doubleWhole).test(6.0) + ":" +
      doublePositive.or(doubleWhole).test(0.0) + ":" +
      doubleWhole.negate().test(2.5)
  )
