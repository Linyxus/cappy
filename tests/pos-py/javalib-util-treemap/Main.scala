import java.util.Map
import java.util.TreeMap

final class RankedKey(val value: Int) extends Comparable[RankedKey]:
  def compareTo(other: RankedKey): Int =
    if value < other.value then -1
    else if value > other.value then 1
    else 0

  override def toString(): String =
    String.valueOf(value)

private def keyOf(value: Int): RankedKey =
  new RankedKey(value)

private def joinEntries(map: Map[RankedKey, String]): String =
  val iter = map.entrySet().iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    val entry = iter.next()
    if !first then
      result += ","
    result += String.valueOf(entry.getKey()) + "=" + entry.getValue()
    first = false
  result

@main def javalibUtilTreeMap(): Unit =
  val map = new TreeMap[RankedKey, String]()
  map.put(keyOf(3), "c")
  map.put(keyOf(1), "a")
  map.put(keyOf(2), "b")

  println("order:" + joinEntries(map))
  println("bounds:" + map.firstKey() + ":" + map.lastKey() + ":" + map.lowerKey(keyOf(2)) + ":" + map.floorKey(keyOf(2)) + ":" + map.ceilingKey(keyOf(2)) + ":" + map.higherKey(keyOf(2)))
  println("sub:" + joinEntries(map.subMap(keyOf(1), true, keyOf(2), true)))
