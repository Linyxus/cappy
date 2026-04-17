import java.util.function.*

@main def javalibFunctionsSpecializedLongMain(): Unit =
  var consumerTotal = 0L
  val longConsumer: LongConsumer = value => consumerTotal += value
  longConsumer
    .andThen(value => consumerTotal += value * 10L)
    .andThen(value => consumerTotal += value * 100L)
    .accept(3L)

  val longFunction: LongFunction[String] = value => "l" + value
  val longSupplier: LongSupplier = () => 23L
  val longToDouble: LongToDoubleFunction = value => value / 2.0
  val longToInt: LongToIntFunction = value => value.toInt + 3
  val longUnary: LongUnaryOperator = value => value + 2L
  val longBinary: LongBinaryOperator = (left, right) => left * 1000L + right
  val toLong: ToLongFunction[String] = value => value.length.toLong
  val toLongBi: ToLongBiFunction[String, String] = (left, right) => (left.length * 10 + right.length).toLong

  val unaryChain =
    longUnary
      .compose(value => value * 3L)
      .compose(LongUnaryOperator.identity())
      .andThen(value => value - 1L)
      .andThen(LongUnaryOperator.identity())
      .andThen(value => value + 7L)

  println("long-consumer-chain:" + consumerTotal)
  println("long-function:" + longFunction.apply(8L))
  println("long-supplier:" + longSupplier.getAsLong())
  println("long-to-double:" + longToDouble.applyAsDouble(9L))
  println("long-to-int:" + longToInt.applyAsInt(5L))
  println("long-unary-chain:" + unaryChain.applyAsLong(4L))
  println("long-unary-identity:" + LongUnaryOperator.identity().applyAsLong(12L))
  println("long-binary:" + longBinary.applyAsLong(3L, 4L))
  println("to-long:" + toLong.applyAsLong("four"))
  println("to-long-bi:" + toLongBi.applyAsLong("ab", "cdefg"))
