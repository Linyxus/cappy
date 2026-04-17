import java.util.function.*

@main def javalibFunctionsSpecializedDoubleMain(): Unit =
  var consumerTotal = 0.0
  val doubleConsumer: DoubleConsumer = value => consumerTotal += value
  doubleConsumer
    .andThen(value => consumerTotal += value * 2.0)
    .andThen(value => consumerTotal += value * 3.0)
    .accept(1.5)

  val doubleFunction: DoubleFunction[String] = value => "d" + value
  val doubleSupplier: DoubleSupplier = () => 2.25
  val doubleToInt: DoubleToIntFunction = value => value.toInt + 4
  val doubleToLong: DoubleToLongFunction = value => value.toLong + 6L
  val doubleUnary: DoubleUnaryOperator = value => value * 1.5
  val doubleBinary: DoubleBinaryOperator = (left, right) => left * 10.0 + right
  val toDouble: ToDoubleFunction[String] = value => value.length + 0.5
  val toDoubleBi: ToDoubleBiFunction[String, String] = (left, right) => left.length * 10.0 + right.length + 0.25

  val unaryChain =
    doubleUnary
      .compose(value => value + 1.0)
      .compose(DoubleUnaryOperator.identity())
      .andThen(value => value - 0.5)
      .andThen(DoubleUnaryOperator.identity())
      .andThen(value => value / 2.0)

  println("double-consumer-chain:" + consumerTotal)
  println("double-function:" + doubleFunction.apply(2.5))
  println("double-supplier:" + doubleSupplier.getAsDouble())
  println("double-to-int:" + doubleToInt.applyAsInt(3.5))
  println("double-to-long:" + doubleToLong.applyAsLong(5.5))
  println("double-unary-chain:" + unaryChain.applyAsDouble(3.0))
  println("double-unary-identity:" + DoubleUnaryOperator.identity().applyAsDouble(8.25))
  println("double-binary:" + doubleBinary.applyAsDouble(1.25, 2.25))
  println("to-double:" + toDouble.applyAsDouble("hey"))
  println("to-double-bi:" + toDoubleBi.applyAsDouble("ab", "cdef"))
