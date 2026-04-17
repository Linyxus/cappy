import java.util.Comparator
import java.util.function.*

final class FunctionComparator extends Comparator[Int]:
  def compare(o1: Int, o2: Int): Int =
    if o1 < o2 then -1
    else if o1 > o2 then 1
    else 0

@main def javalibFunctionGenericFunctions(): Unit =
  val function: Function[Int, Int] = value => value + 1
  val biFunction: BiFunction[Int, Int, Int] = (left, right) => left + right
  val unary: UnaryOperator[String] = value => value + "!"
  val comparator = new FunctionComparator

  val composed: Function[Int, Int] =
    function.andThen[Int](value => value * 2).compose[Int](value => value + 3)
  println("function:" + composed.apply(4))
  println("bifunction:" + biFunction.andThen[Int](value => value * 2).apply(2, 3))
  println("unary:" + unary.andThen(UnaryOperator.identity[String]()).apply("go"))
  println(
    "binary:" +
      BinaryOperator.minBy(comparator).apply(4, 2) + ":" +
      BinaryOperator.maxBy(comparator).apply(4, 2)
  )
