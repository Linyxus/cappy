import java.util.function.*

@main def javalibFunctionPredicates(): Unit =
  val startsWithA: Predicate[String] = value => value.startsWith("a")
  val longerThanTwo: Predicate[String] = value => value.length > 2

  val bothPositive: BiPredicate[Int, Int] = (left, right) => left > 0 && right > 0
  val sumEven: BiPredicate[Int, Int] = (left, right) => (left + right) % 2 == 0

  val intPositive: IntPredicate = value => value > 0
  val intEven: IntPredicate = value => value % 2 == 0

  val longPositive: LongPredicate = value => value > 0L
  val longZero: LongPredicate = value => value == 0L

  val doublePositive: DoublePredicate = value => value > 0.0
  val doubleIsOne: DoublePredicate = value => value == 1.0

  println("predicate:" + startsWithA.and(longerThanTwo).or(value => value.endsWith("!")).test("abcd"))
  println("bipredicate:" + bothPositive.and(sumEven).or((left, right) => left == right).test(1, 3))
  println("int-predicate:" + intPositive.and(intEven).test(4))
  println("long-predicate:" + longPositive.or(longZero).test(0L))
  println("double-predicate:" + doublePositive.and(doubleIsOne.negate()).test(2.5))
