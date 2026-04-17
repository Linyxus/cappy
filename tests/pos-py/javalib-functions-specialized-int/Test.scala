import java.util.function.*

@main def javalibFunctionsSpecializedIntMain(): Unit =
  var consumerTotal = 0
  val intConsumer: IntConsumer = value => consumerTotal += value
  intConsumer
    .andThen(value => consumerTotal += value * 10)
    .andThen(value => consumerTotal += value * 100)
    .accept(2)

  val intFunction: IntFunction[String] = value => "n" + value
  val intSupplier: IntSupplier = () => 17
  val intToDouble: IntToDoubleFunction = value => value / 2.0
  val intToLong: IntToLongFunction = value => value.toLong + 4L
  val intUnary: IntUnaryOperator = value => value + 1
  val intBinary: IntBinaryOperator = (left, right) => left * 100 + right
  val toInt: ToIntFunction[String] = value => value.length
  val toIntBi: ToIntBiFunction[String, String] = (left, right) => left.length * 10 + right.length

  val unaryChain =
    intUnary
      .compose(value => value * 2)
      .compose(IntUnaryOperator.identity())
      .andThen(value => value - 3)
      .andThen(IntUnaryOperator.identity())
      .andThen(value => value * value)

  println("int-consumer-chain:" + consumerTotal)
  println("int-function:" + intFunction.apply(5))
  println("int-supplier:" + intSupplier.getAsInt())
  println("int-to-double:" + intToDouble.applyAsDouble(5))
  println("int-to-long:" + intToLong.applyAsLong(5))
  println("int-unary-chain:" + unaryChain.applyAsInt(4))
  println("int-unary-identity:" + IntUnaryOperator.identity().applyAsInt(8))
  println("int-binary:" + intBinary.applyAsInt(3, 4))
  println("to-int:" + toInt.applyAsInt("scala"))
  println("to-int-bi:" + toIntBi.applyAsInt("ab", "xyz"))
