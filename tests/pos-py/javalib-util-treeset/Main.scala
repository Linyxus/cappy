import java.util.Set
import java.util.TreeSet

final class RankedValue(val value: Int) extends Comparable[RankedValue]:
  def compareTo(other: RankedValue): Int =
    if value < other.value then -1
    else if value > other.value then 1
    else 0

  override def toString(): String =
    String.valueOf(value)

private def valueOf(n: Int): RankedValue =
  new RankedValue(n)

private def joinSet(set: Set[RankedValue]): String =
  val iter = set.iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iter.next())
    first = false
  result

@main def javalibUtilTreeSet(): Unit =
  val set = new TreeSet[RankedValue]()
  set.add(valueOf(3))
  set.add(valueOf(1))
  set.add(valueOf(2))

  println("order:" + joinSet(set))
  println("subset:" + joinSet(set.tailSet(valueOf(2), true)))
  println("poll:" + set.pollFirst() + ":" + set.pollLast() + ":" + set.size())
