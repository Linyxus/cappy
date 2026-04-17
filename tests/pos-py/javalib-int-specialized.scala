import java.util.function.*

@main def javalibFunctionIntSpecialized(): Unit =
  var consumerTotal = 0
  val intConsumer: IntConsumer = value => consumerTotal += value
  intConsumer.andThen(value => consumerTotal += value * 10).accept(2)

  val intFunction: IntFunction[String] = value => "n" + value
  val intSupplier: IntSupplier = () => 17
  val intToDouble: IntToDoubleFunction = value => value / 2.0
  val intToLong: IntToLongFunction = value => value.toLong + 4L
  val intUnary: IntUnaryOperator = value => value + 1
  val intBinary: IntBinaryOperator = (left, right) => left + right
  val toInt: ToIntFunction[String] = value => value.length
  val toIntBi: ToIntBiFunction[String, String] = (left, right) => left.length + right.length

  println("int-consumer:" + consumerTotal)
  println("int-function:" + intFunction.apply(5))
  println("int-supplier:" + intSupplier.getAsInt())
  println("int-to-double:" + intToDouble.applyAsDouble(5))
  println("int-to-long:" + intToLong.applyAsLong(5))
  println("int-unary:" + intUnary.compose(value => value * 2).andThen(value => value - 3).applyAsInt(4))
  println("int-binary:" + intBinary.applyAsInt(3, 4))
  println("to-int:" + toInt.applyAsInt("hey"))
  println("to-int-bi:" + toIntBi.applyAsInt("ab", "cde"))
