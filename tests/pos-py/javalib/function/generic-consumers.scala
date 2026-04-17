import java.util.function.*

@main def javalibFunctionGenericConsumers(): Unit =
  val supplier: Supplier[String] = () => "sup"
  val booleanSupplier: BooleanSupplier = () => true

  var consumerTotal = 0
  val consumer: Consumer[Int] = value => consumerTotal += value
  consumer.andThen(value => consumerTotal += value * 10).accept(2)

  var biConsumerTotal = 0
  val biConsumer: BiConsumer[Int, Int] = (left, right) => biConsumerTotal = left + right
  biConsumer.andThen((left, right) => biConsumerTotal += left * right).accept(2, 3)

  var objInt = ""
  val objIntConsumer: ObjIntConsumer[String] = (value, count) => objInt = value + count

  var objLong = ""
  val objLongConsumer: ObjLongConsumer[String] = (value, count) => objLong = value + count

  var objDouble = ""
  val objDoubleConsumer: ObjDoubleConsumer[String] = (value, count) => objDouble = value + count

  objIntConsumer.accept("age", 7)
  objLongConsumer.accept("id", 9L)
  objDoubleConsumer.accept("pi", 3.5)

  println("supplier:" + supplier.get())
  println("boolean:" + booleanSupplier.getAsBoolean())
  println("consumer:" + consumerTotal)
  println("biconsumer:" + biConsumerTotal)
  println("obj-int:" + objInt)
  println("obj-long:" + objLong)
  println("obj-double:" + objDouble)
