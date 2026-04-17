import java.util.Comparator
import java.util.function.*

final class WeightedValue(val label: String, val weight: Int)

@main def javalibFunctionsGeneric(): Unit =
  val weightKey: ToIntFunction[WeightedValue] = value => value.weight
  val supplier: Supplier[String] = () => "alpha"
  val booleanSupplier: BooleanSupplier = () => 3 < 5

  var consumerTrace = ""
  val consumer: Consumer[String] = value => consumerTrace = consumerTrace + value
  consumer.andThen(value => consumerTrace = consumerTrace + ":" + value.length).accept("go")

  var biConsumerTrace = ""
  val biConsumer: BiConsumer[String, Int] = (label, count) => biConsumerTrace = label + count
  biConsumer.andThen((label, count) => biConsumerTrace = biConsumerTrace + ":" + (count * 2)).accept("n", 4)

  val increment: Function[Int, Int] = value => value + 1
  val functionChain = increment.compose[Int](value => value - 3).andThen[Int](value => value * 5)
  val pairSum: BiFunction[Int, Int, Int] = (left, right) => left + right

  val light = new WeightedValue("light", 2)
  val heavy = new WeightedValue("heavy", 7)
  val byWeight: Comparator[WeightedValue] = Comparator.comparingInt[WeightedValue](weightKey)

  var objIntTrace = ""
  val objIntConsumer: ObjIntConsumer[String] = (label, count) => objIntTrace = label + ":" + count

  var objLongTrace = ""
  val objLongConsumer: ObjLongConsumer[String] = (label, count) => objLongTrace = label + ":" + count

  var objDoubleTrace = ""
  val objDoubleConsumer: ObjDoubleConsumer[String] = (label, count) => objDoubleTrace = label + ":" + count

  objIntConsumer.accept("age", 7)
  objLongConsumer.accept("id", 9L)
  objDoubleConsumer.accept("pi", 3.5)

  println("supplier:" + supplier.get())
  println("boolean-supplier:" + booleanSupplier.getAsBoolean())
  println("consumer-andThen:" + consumerTrace)
  println("biconsumer-andThen:" + biConsumerTrace)
  println("function-chain:" + functionChain.apply(10))
  println("function-identity:" + Function.identity[String]().apply("id"))
  println("bifunction-andThen:" + pairSum.andThen[Int](value => value * 3).apply(2, 4))
  println("unary-identity:" + UnaryOperator.identity[String]().apply("echo"))
  println(
    "binary-ops:" +
      BinaryOperator.minBy(byWeight).apply(light, heavy).label + ":" +
      BinaryOperator.maxBy(byWeight).apply(light, heavy).label
  )
  println("obj-int:" + objIntTrace)
  println("obj-long:" + objLongTrace)
  println("obj-double:" + objDoubleTrace)
