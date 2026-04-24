import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet

case class Ranked(label: String, rank: Int)

given Ordering[Ranked] with
  def compare(left: Ranked, right: Ranked): Int =
    val rankCompare =
      if left.rank < right.rank then -1
      else if left.rank > right.rank then 1
      else 0
    if rankCompare != 0 then rankCompare
    else left.label.compareTo(right.label)

@main def scalaCollectionsOrdering(): Unit =
  val treeSet = TreeSet(3, 1, 2, 2)
  println("treeset:" + treeSet.head + ":" + treeSet.last + ":" + treeSet.size)

  val treeMap = TreeMap(2 -> "b", 1 -> "a")
  println("treemap:" + treeMap.firstKey + ":" + treeMap.lastKey + ":" + treeMap.size)

  val sortedSet: SortedSet[Int] = SortedSet(5, 4, 6)
  val sortedSet2 = sortedSet + 3 - 5
  val sortedSetRange = sortedSet.rangeFrom(5)
  println("sortedset:" + sortedSet.head + ":" + sortedSet.last + ":" + sortedSet2.head + ":" + sortedSet2.last + ":" + sortedSetRange.head + ":" + sortedSetRange.last + ":" + sortedSetRange.size)

  val sortedMap: SortedMap[String, Int] = SortedMap("b" -> 2, "a" -> 1)
  println("sortedmap:" + sortedMap.firstKey + ":" + sortedMap.lastKey + ":" + sortedMap.size)

  val ranked = TreeSet(Ranked("low", 2), Ranked("top", 1), Ranked("tie", 1))
  println("custom-case:" + ranked.head.label + ":" + ranked.head.rank + ":" + ranked.last.label + ":" + ranked.last.rank + ":" + ranked.size)
